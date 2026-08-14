#include <cpp11.hpp>
#include <vector>
#include <string>
#include <cstring>
#include <cstdint>

// Helpers for the classic (non-0xfd) HAR container format:
// a flat sequence of [int32 length][payload][int32 length] records,
// where a 4-byte non-blank payload names the header that owns the
// records which follow it.

namespace {

inline std::string trim_ws(const char* s, int n) {
  int b = 0, e = n;
  while (b < e && (s[b] == ' ' || s[b] == '\t' || s[b] == '\r' || s[b] == '\n')) ++b;
  while (e > b && (s[e - 1] == ' ' || s[e - 1] == '\t' || s[e - 1] == '\r' || s[e - 1] == '\n')) --e;
  return std::string(s + b, e - b);
}

} // namespace

// Split the raw bytes of a classic-format HAR file into a named list:
// one element per header, each a list of raw records (the header-name
// record first), mirroring the R record-splitting loops byte for byte.
[[cpp11::register]]
cpp11::list har_split_records(cpp11::raws cf) {
  const uint8_t* p = RAW((SEXP)cf);
  const R_xlen_t n = cf.size();

  std::vector<std::string> names;
  std::vector<std::vector<std::pair<R_xlen_t, int32_t>>> recs;
  int cur = -1;

  R_xlen_t i = 0;
  while (i + 4 <= n) {
    int32_t len;
    std::memcpy(&len, p + i, 4);
    if (len < 0 || i + 4 + len + 4 > n) {
      cpp11::stop("malformed HAR record at byte %lld", (long long)(i + 1));
    }
    i += 4;
    if (len == 4) {
      const bool all_space = p[i] == 0x20 && p[i + 1] == 0x20 &&
                             p[i + 2] == 0x20 && p[i + 3] == 0x20;
      if (!all_space) {
        names.push_back(trim_ws(reinterpret_cast<const char*>(p + i), 4));
        recs.emplace_back();
        cur = static_cast<int>(names.size()) - 1;
      }
    }
    if (cur >= 0) recs[cur].emplace_back(i, len);
    int32_t len2;
    std::memcpy(&len2, p + i + len, 4);
    if (len2 != len) {
      Rf_warning("toRead different from hasRead in %s",
                 cur >= 0 ? names[cur].c_str() : "?");
    }
    i += len + 4;
  }

  cpp11::writable::list out(static_cast<R_xlen_t>(names.size()));
  cpp11::writable::strings out_names(static_cast<R_xlen_t>(names.size()));
  for (size_t h = 0; h < names.size(); ++h) {
    out_names[static_cast<R_xlen_t>(h)] = names[h];
    cpp11::writable::list rl(static_cast<R_xlen_t>(recs[h].size()));
    for (size_t r = 0; r < recs[h].size(); ++r) {
      const R_xlen_t off = recs[h][r].first;
      const int32_t len = recs[h][r].second;
      SEXP rv = PROTECT(Rf_allocVector(RAWSXP, len));
      std::memcpy(RAW(rv), p + off, len);
      rl[static_cast<R_xlen_t>(r)] = rv;
      UNPROTECT(1);
    }
    out[static_cast<R_xlen_t>(h)] = rl;
  }
  out.names() = out_names;
  return out;
}

// Concatenate records[[k]][(offset+1):length] across a list of raw records.
[[cpp11::register]]
cpp11::raws har_payload_concat(cpp11::list records, int offset) {
  const R_xlen_t nr = records.size();
  R_xlen_t total = 0;
  for (R_xlen_t r = 0; r < nr; ++r) {
    const R_xlen_t len = Rf_xlength(records[r]);
    if (len > offset) total += len - offset;
  }
  cpp11::writable::raws out(total);
  uint8_t* po = RAW((SEXP)out);
  R_xlen_t at = 0;
  for (R_xlen_t r = 0; r < nr; ++r) {
    const R_xlen_t len = Rf_xlength(records[r]);
    if (len <= offset) continue;
    std::memcpy(po + at, RAW(records[r]) + offset, len - offset);
    at += len - offset;
  }
  return out;
}

namespace {

// Stream payload bytes (records[[k]][(offset+1):length]) through a fixed-width
// decoder, carrying partial words across record boundaries exactly as if the
// payloads had been concatenated first. Emits at most n_max values.
template <typename Emit>
void payload_stream(cpp11::list& records, int offset, R_xlen_t n_max,
                    int word, Emit emit) {
  uint8_t carry[8];
  int cn = 0;
  R_xlen_t emitted = 0;
  const R_xlen_t nr = records.size();
  for (R_xlen_t r = 0; r < nr && emitted < n_max; ++r) {
    const R_xlen_t len = Rf_xlength(records[r]);
    if (len <= offset) continue;
    const uint8_t* p = RAW(records[r]) + offset;
    R_xlen_t avail = len - offset;
    R_xlen_t j = 0;
    if (cn > 0) {
      while (cn < word && j < avail) carry[cn++] = p[j++];
      if (cn == word) {
        emit(carry, emitted++);
        cn = 0;
        if (emitted == n_max) return;
      }
    }
    const R_xlen_t words = (avail - j) / word;
    const R_xlen_t take = words < (n_max - emitted) ? words : (n_max - emitted);
    for (R_xlen_t k = 0; k < take; ++k) {
      emit(p + j + static_cast<R_xlen_t>(word) * k, emitted++);
    }
    j += static_cast<R_xlen_t>(word) * words;
    while (j < avail && cn < word) carry[cn++] = p[j++];
  }
}

// Total payload bytes past `offset` across a record list.
R_xlen_t payload_bytes(cpp11::list& records, int offset) {
  R_xlen_t total = 0;
  const R_xlen_t nr = records.size();
  for (R_xlen_t r = 0; r < nr; ++r) {
    const R_xlen_t len = Rf_xlength(records[r]);
    if (len > offset) total += len - offset;
  }
  return total;
}

} // namespace

// Fused concatenate-and-convert of float32 record payloads to doubles,
// replacing har_payload_concat() + readBin(..., "double", size = 4).
// Returns min(n, available words) values, like readBin's short-read.
[[cpp11::register]]
cpp11::doubles har_payload_f32(cpp11::list records, int offset, double n) {
  R_xlen_t m = payload_bytes(records, offset) / 4;
  if (static_cast<R_xlen_t>(n) < m) m = static_cast<R_xlen_t>(n);
  cpp11::writable::doubles out(m);
  double* po = REAL(out);
  payload_stream(records, offset, m, 4, [po](const uint8_t* b, R_xlen_t at) {
    float v;
    std::memcpy(&v, b, 4);
    po[at] = v;
  });
  return out;
}

// Fused concatenate-and-convert of int32 record payloads to integers,
// replacing har_payload_concat() + readBin(..., "integer", size = 4).
[[cpp11::register]]
cpp11::integers har_payload_i32(cpp11::list records, int offset, double n) {
  R_xlen_t m = payload_bytes(records, offset) / 4;
  if (static_cast<R_xlen_t>(n) < m) m = static_cast<R_xlen_t>(n);
  cpp11::writable::integers out(m);
  int* po = INTEGER(out);
  payload_stream(records, offset, m, 4, [po](const uint8_t* b, R_xlen_t at) {
    int32_t v;
    std::memcpy(&v, b, 4);
    po[at] = v;
  });
  return out;
}

// Scatter sparse RESPSE records into a dense double vector of length n.
// Each record payload after `offset` holds k int32 1-based locations
// followed by k float32 values.
[[cpp11::register]]
cpp11::doubles har_spse_fill(cpp11::list records, int offset, double n) {
  const R_xlen_t nt = static_cast<R_xlen_t>(n);
  cpp11::writable::doubles out(nt);
  double* po = REAL(out);
  std::memset(po, 0, sizeof(double) * nt);

  const R_xlen_t nr = records.size();
  for (R_xlen_t r = 0; r < nr; ++r) {
    const R_xlen_t len = Rf_xlength(records[r]);
    if (len <= offset) continue;
    const uint8_t* p = RAW(records[r]) + offset;
    const R_xlen_t points = (len - offset) / 8;
    const uint8_t* pv = p + 4 * points;
    for (R_xlen_t j = 0; j < points; ++j) {
      int32_t loc;
      float val;
      std::memcpy(&loc, p + 4 * j, 4);
      std::memcpy(&val, pv + 4 * j, 4);
      if (loc >= 1 && loc <= nt) po[loc - 1] = val;
    }
  }
  return out;
}

// Decode fixed-width character fields from raw bytes: length(bytes)/width
// strings, NUL bytes read as spaces, optionally trimmed like trimws().
[[cpp11::register]]
cpp11::strings har_fixed_width_strings(cpp11::raws bytes, int width, bool trim) {
  const uint8_t* p = RAW((SEXP)bytes);
  const R_xlen_t n = bytes.size() / width;
  cpp11::writable::strings out(n);
  std::string buf(width, ' ');
  for (R_xlen_t s = 0; s < n; ++s) {
    for (int j = 0; j < width; ++j) {
      const uint8_t b = p[s * width + j];
      buf[j] = (b == 0x00) ? ' ' : static_cast<char>(b);
    }
    if (trim) {
      out[s] = trim_ws(buf.data(), width);
    } else {
      out[s] = buf;
    }
  }
  return out;
}
