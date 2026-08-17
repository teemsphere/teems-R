#include <cpp11.hpp>
#include <fstream>
#include <vector>
#include <string>
#include <set>
#include <numeric>
#include <cstring>
#include <cstdint>
#include <algorithm>

// Constants matching teems-solver/teems-parser/export_solution.c
#define NAMESIZE 256
#define TABREADLINE 20000
#define HEADERSIZE 5
#define MAXVARDIM 10
#define MAXSUPSET 12

typedef int uvdim;
typedef int64_t uvadd;
typedef double forreal;
typedef float ha_floattype;

// Struct definitions matching the solver binary format exactly

struct ha_cgeset {
  char header[HEADERSIZE];
  int fileid;
  char setname[NAMESIZE];
  char readele[TABREADLINE];
  uvadd begadd;
  uvdim size;
  uvdim subsetid[MAXSUPSET];
  bool intertemp;
  int intsup;
  bool regional;
  int regsup;
};

struct ha_cgesetele {
  char setele[NAMESIZE];
  uvdim setsh[MAXSUPSET];
};

struct hcge_cof {
  char cofname[NAMESIZE];
  uvadd begadd;
  uvdim size;
  uvadd setid[MAXVARDIM];
  uvadd antidims[MAXVARDIM];
  uvadd matsize;
  bool level_par;
  bool change_real;
  bool suplval;
  int gltype;
  ha_floattype glval;
};

// Returns the number of active (non-zero) entries from the front of a
// fixed-length integer array, trimming trailing zeros.
template<typename T, int N>
static int active_count(const T (&arr)[N]) {
  int last = 0;
  for (int k = 0; k < N; k++) {
    if (arr[k] != 0) last = k + 1;
  }
  return last;
}

// Packs the first `n` elements of a fixed-length array into a comma string.
template<typename T, int N>
static std::string pack_n(const T (&arr)[N], int n) {
  std::string s;
  for (int k = 0; k < n; k++) {
    if (k > 0) s += ",";
    s += std::to_string(arr[k]);
  }
  return s;
}

// --- parse_solution_meta: reads .mds, .var, .sel, .set (no .bin) ---

[[cpp11::register]]
cpp11::list parse_solution_meta(std::string path_prefix) {

  // --- Read .mds file: 4 long ints ---
  std::string mds_path = path_prefix + "mds";
  std::ifstream mds_file(mds_path, std::ios::binary);
  if (!mds_file.is_open()) {
    cpp11::stop("Cannot open .mds file: %s", mds_path.c_str());
  }
  uvadd modeldes[4];
  mds_file.read(reinterpret_cast<char*>(modeldes), sizeof(uvadd) * 4);
  mds_file.close();

  uvadd nsetspace = modeldes[0];
  uvadd nvar      = modeldes[1];
  uvadd nset      = modeldes[3];

  // --- Read .var file: nvar hcge_cof structs ---
  std::string var_path = path_prefix + "var";
  std::ifstream var_file(var_path, std::ios::binary);
  if (!var_file.is_open()) {
    cpp11::stop("Cannot open .var file: %s", var_path.c_str());
  }
  std::vector<hcge_cof> var_structs(nvar);
  var_file.read(reinterpret_cast<char*>(var_structs.data()),
                sizeof(hcge_cof) * nvar);
  var_file.close();

  using namespace cpp11::literals;

  cpp11::writable::strings var_cofname(static_cast<R_xlen_t>(nvar));
  cpp11::writable::doubles var_begadd(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_size(static_cast<R_xlen_t>(nvar));
  cpp11::writable::doubles var_matsize(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_level_par(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_change_real(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_suplval(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_gltype(static_cast<R_xlen_t>(nvar));
  cpp11::writable::doubles var_glval(static_cast<R_xlen_t>(nvar));
  cpp11::writable::strings var_setid(static_cast<R_xlen_t>(nvar));
  cpp11::writable::strings var_antidims(static_cast<R_xlen_t>(nvar));

  for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(nvar); i++) {
    var_cofname[i] = std::string(var_structs[i].cofname);
    var_begadd[i]  = static_cast<double>(var_structs[i].begadd);
    var_size[i]    = var_structs[i].size;
    var_matsize[i] = static_cast<double>(var_structs[i].matsize);
    var_level_par[i]   = static_cast<int>(var_structs[i].level_par);
    var_change_real[i] = static_cast<int>(var_structs[i].change_real);
    var_suplval[i]     = static_cast<int>(var_structs[i].suplval);
    var_gltype[i]      = var_structs[i].gltype;
    var_glval[i]       = static_cast<double>(var_structs[i].glval);

    int ndim = active_count(var_structs[i].antidims);
    var_setid[i]    = pack_n(var_structs[i].setid,    ndim);
    var_antidims[i] = pack_n(var_structs[i].antidims, ndim);
  }

  cpp11::writable::list var_list(
    {"cofname"_nm = (SEXP)var_cofname,
     "begadd"_nm = (SEXP)var_begadd,
     "size"_nm = (SEXP)var_size,
     "setid"_nm = (SEXP)var_setid,
     "antidims"_nm = (SEXP)var_antidims,
     "matsize"_nm = (SEXP)var_matsize,
     "level_par"_nm = (SEXP)var_level_par,
     "change_real"_nm = (SEXP)var_change_real,
     "suplval"_nm = (SEXP)var_suplval,
     "gltype"_nm = (SEXP)var_gltype,
     "glval"_nm = (SEXP)var_glval}
  );

  // --- Read .sel file: nsetspace ha_cgesetele structs ---
  std::string sel_path = path_prefix + "sel";
  std::ifstream sel_file(sel_path, std::ios::binary);
  if (!sel_file.is_open()) {
    cpp11::stop("Cannot open .sel file: %s", sel_path.c_str());
  }
  std::vector<ha_cgesetele> sel_structs(nsetspace);
  sel_file.read(reinterpret_cast<char*>(sel_structs.data()),
                sizeof(ha_cgesetele) * nsetspace);
  sel_file.close();

  cpp11::writable::strings sel_setele(static_cast<R_xlen_t>(nsetspace));
  cpp11::writable::strings sel_setsh(static_cast<R_xlen_t>(nsetspace));

  for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(nsetspace); i++) {
    sel_setele[i] = std::string(sel_structs[i].setele);
    sel_setsh[i] = pack_n(sel_structs[i].setsh, active_count(sel_structs[i].setsh));
  }

  cpp11::writable::list sel_list(
    {"setele"_nm = (SEXP)sel_setele,
     "setsh"_nm = (SEXP)sel_setsh}
  );

  // --- Read .set file: nset ha_cgeset structs ---
  std::string set_path = path_prefix + "set";
  std::ifstream set_file(set_path, std::ios::binary);
  if (!set_file.is_open()) {
    cpp11::stop("Cannot open .set file: %s", set_path.c_str());
  }
  std::vector<ha_cgeset> set_structs(nset);
  set_file.read(reinterpret_cast<char*>(set_structs.data()),
                sizeof(ha_cgeset) * nset);
  set_file.close();

  cpp11::writable::strings set_header(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_fileid(static_cast<R_xlen_t>(nset));
  cpp11::writable::strings set_setname(static_cast<R_xlen_t>(nset));
  cpp11::writable::strings set_readele(static_cast<R_xlen_t>(nset));
  cpp11::writable::doubles set_begadd(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_size(static_cast<R_xlen_t>(nset));
  cpp11::writable::strings set_subsetid(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_intertemp(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_intsup(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_regional(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_regsup(static_cast<R_xlen_t>(nset));

  for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(nset); i++) {
    set_header[i]    = std::string(set_structs[i].header);
    set_fileid[i]    = set_structs[i].fileid;
    set_setname[i]   = std::string(set_structs[i].setname);
    set_readele[i]   = std::string(set_structs[i].readele);
    set_begadd[i]    = static_cast<double>(set_structs[i].begadd);
    set_size[i]      = set_structs[i].size;
    set_intertemp[i] = static_cast<int>(set_structs[i].intertemp);
    set_intsup[i]    = set_structs[i].intsup;
    set_regional[i]  = static_cast<int>(set_structs[i].regional);
    set_regsup[i]    = set_structs[i].regsup;

    set_subsetid[i] = pack_n(set_structs[i].subsetid, active_count(set_structs[i].subsetid));
  }

  cpp11::writable::list set_list(
    {"header"_nm = (SEXP)set_header,
     "fileid"_nm = (SEXP)set_fileid,
     "setname"_nm = (SEXP)set_setname,
     "readele"_nm = (SEXP)set_readele,
     "begadd"_nm = (SEXP)set_begadd,
     "size"_nm = (SEXP)set_size,
     "subsetid"_nm = (SEXP)set_subsetid,
     "intertemp"_nm = (SEXP)set_intertemp,
     "intsup"_nm = (SEXP)set_intsup,
     "regional"_nm = (SEXP)set_regional,
     "regsup"_nm = (SEXP)set_regsup}
  );

  cpp11::writable::list result(
    {"var"_nm = (SEXP)var_list,
     "sel"_nm = (SEXP)sel_list,
     "set"_nm = (SEXP)set_list}
  );

  return result;
}

// --- parse_solution_bins: reads .mds, .var, and .bin selectively ---
// names_filter: character vector of variable names to extract.
// Empty vector reads all variables.

[[cpp11::register]]
cpp11::list parse_solution_bins(std::string path_prefix, cpp11::strings names_filter) {

  // --- Read .mds ---
  std::string mds_path = path_prefix + "mds";
  std::ifstream mds_file(mds_path, std::ios::binary);
  if (!mds_file.is_open()) {
    cpp11::stop("Cannot open .mds file: %s", mds_path.c_str());
  }
  uvadd modeldes[4];
  mds_file.read(reinterpret_cast<char*>(modeldes), sizeof(uvadd) * 4);
  mds_file.close();

  uvadd nvar    = modeldes[1];
  uvadd nvarele = modeldes[2];

  // --- Read .var ---
  std::string var_path = path_prefix + "var";
  std::ifstream var_file(var_path, std::ios::binary);
  if (!var_file.is_open()) {
    cpp11::stop("Cannot open .var file: %s", var_path.c_str());
  }
  std::vector<hcge_cof> var_structs(nvar);
  var_file.read(reinterpret_cast<char*>(var_structs.data()),
                sizeof(hcge_cof) * nvar);
  var_file.close();

  // --- Determine selection ---
  bool filter = (names_filter.size() > 0);
  std::vector<R_xlen_t> sel_idx;

  if (filter) {
    std::set<std::string> name_set;
    for (R_xlen_t k = 0; k < names_filter.size(); k++) {
      name_set.insert(std::string(names_filter[k]));
    }
    for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(nvar); i++) {
      if (name_set.count(std::string(var_structs[i].cofname))) {
        sel_idx.push_back(i);
      }
    }
  } else {
    sel_idx.resize(static_cast<size_t>(nvar));
    std::iota(sel_idx.begin(), sel_idx.end(), 0);
  }

  // --- Read .bin selectively ---
  uvadd total_ele = 0;
  for (auto i : sel_idx) total_ele += var_structs[i].matsize;

  std::string bin_path = path_prefix + "bin";
  std::ifstream bin_file(bin_path, std::ios::binary);
  if (!bin_file.is_open()) {
    cpp11::stop("Cannot open .bin file: %s", bin_path.c_str());
  }

  cpp11::writable::doubles bin_vec(static_cast<R_xlen_t>(total_ele));

  if (filter) {
    R_xlen_t write_pos = 0;
    for (auto i : sel_idx) {
      uvadd beg = var_structs[i].begadd;
      uvadd mat = var_structs[i].matsize;
      bin_file.seekg(static_cast<std::streamoff>(beg) * sizeof(forreal),
                     std::ios::beg);
      bin_file.read(reinterpret_cast<char*>(REAL(bin_vec) + write_pos),
                    sizeof(forreal) * mat);
      write_pos += static_cast<R_xlen_t>(mat);
    }
  } else {
    bin_file.read(reinterpret_cast<char*>(REAL(bin_vec)),
                  sizeof(forreal) * nvarele);
  }
  if (total_ele > 0 && !bin_file) {
    // an empty .bin is what a probe run (-solmed probe) leaves behind
    cpp11::stop("Truncated or empty .bin file (no solution written): %s",
                bin_path.c_str());
  }
  bin_file.close();

  // --- Build filtered var arrays ---
  R_xlen_t nsel = static_cast<R_xlen_t>(sel_idx.size());

  using namespace cpp11::literals;

  cpp11::writable::strings var_cofname(nsel);
  cpp11::writable::doubles var_begadd(nsel);
  cpp11::writable::integers var_size(nsel);
  cpp11::writable::doubles var_matsize(nsel);
  cpp11::writable::integers var_level_par(nsel);
  cpp11::writable::integers var_change_real(nsel);
  cpp11::writable::integers var_suplval(nsel);
  cpp11::writable::integers var_gltype(nsel);
  cpp11::writable::doubles var_glval(nsel);
  cpp11::writable::strings var_setid(nsel);
  cpp11::writable::strings var_antidims(nsel);

  for (R_xlen_t j = 0; j < nsel; j++) {
    R_xlen_t i = sel_idx[j];
    var_cofname[j] = std::string(var_structs[i].cofname);
    var_begadd[j]  = static_cast<double>(var_structs[i].begadd);
    var_size[j]    = var_structs[i].size;
    var_matsize[j] = static_cast<double>(var_structs[i].matsize);
    var_level_par[j]   = static_cast<int>(var_structs[i].level_par);
    var_change_real[j] = static_cast<int>(var_structs[i].change_real);
    var_suplval[j]     = static_cast<int>(var_structs[i].suplval);
    var_gltype[j]      = var_structs[i].gltype;
    var_glval[j]       = static_cast<double>(var_structs[i].glval);

    int ndim = active_count(var_structs[i].antidims);
    var_setid[j]    = pack_n(var_structs[i].setid,    ndim);
    var_antidims[j] = pack_n(var_structs[i].antidims, ndim);
  }

  cpp11::writable::list var_list(
    {"cofname"_nm = (SEXP)var_cofname,
     "begadd"_nm = (SEXP)var_begadd,
     "size"_nm = (SEXP)var_size,
     "setid"_nm = (SEXP)var_setid,
     "antidims"_nm = (SEXP)var_antidims,
     "matsize"_nm = (SEXP)var_matsize,
     "level_par"_nm = (SEXP)var_level_par,
     "change_real"_nm = (SEXP)var_change_real,
     "suplval"_nm = (SEXP)var_suplval,
     "gltype"_nm = (SEXP)var_gltype,
     "glval"_nm = (SEXP)var_glval}
  );

  // --- Read .acc if present (embedded-RK cumulative error metrics,
  //     one double per element in .bin order) ---
  cpp11::sexp acc_sexp = R_NilValue;
  std::string acc_path = path_prefix + "acc";
  std::ifstream acc_file(acc_path, std::ios::binary);
  if (acc_file.is_open()) {
    cpp11::writable::doubles acc_vec(static_cast<R_xlen_t>(total_ele));
    if (filter) {
      R_xlen_t write_pos = 0;
      for (auto i : sel_idx) {
        uvadd beg = var_structs[i].begadd;
        uvadd mat = var_structs[i].matsize;
        acc_file.seekg(static_cast<std::streamoff>(beg) * sizeof(forreal),
                       std::ios::beg);
        acc_file.read(reinterpret_cast<char*>(REAL(acc_vec) + write_pos),
                      sizeof(forreal) * mat);
        write_pos += static_cast<R_xlen_t>(mat);
      }
    } else {
      acc_file.read(reinterpret_cast<char*>(REAL(acc_vec)),
                    sizeof(forreal) * nvarele);
    }
    acc_file.close();
    acc_sexp = (SEXP)acc_vec;
  }

  cpp11::writable::list result(
    {"bin"_nm = (SEXP)bin_vec,
     "var"_nm = (SEXP)var_list,
     "acc"_nm = (SEXP)acc_sexp}
  );

  return result;
}

// --- parse_coefficients: reads .cof + .cbin (the coefficient twins of
//     .var + .bin, ROADMAP 6.13). .cof = int64 header {version, ncof,
//     ncofele, reserved} + ncof x hcge_cof + ncof x uint8 kind (bit 0
//     PostSim, bit 1 parameter); .cbin = ncofele x double in begadd order.
// names_filter: coefficient names (solver casing) to extract; empty
// reads all. Values are read only when read_values is true.

[[cpp11::register]]
cpp11::list parse_coefficients(std::string path_prefix, cpp11::strings names_filter,
                               bool read_values) {

  std::string cof_path = path_prefix + "cof";
  std::ifstream cof_file(cof_path, std::ios::binary);
  if (!cof_file.is_open()) {
    cpp11::stop("Cannot open .cof file: %s", cof_path.c_str());
  }
  uvadd hdr[4];
  cof_file.read(reinterpret_cast<char*>(hdr), sizeof(uvadd) * 4);
  if (hdr[0] != 1) {
    cpp11::stop("Unsupported .cof version %lld in %s",
                static_cast<long long>(hdr[0]), cof_path.c_str());
  }
  uvadd ncof    = hdr[1];
  uvadd ncofele = hdr[2];

  std::vector<hcge_cof> cof_structs(ncof);
  cof_file.read(reinterpret_cast<char*>(cof_structs.data()),
                sizeof(hcge_cof) * ncof);
  std::vector<unsigned char> kind(ncof);
  cof_file.read(reinterpret_cast<char*>(kind.data()), ncof);
  if (!cof_file) {
    cpp11::stop("Truncated .cof file: %s", cof_path.c_str());
  }
  cof_file.close();

  bool filter = (names_filter.size() > 0);
  std::vector<R_xlen_t> sel_idx;
  if (filter) {
    std::set<std::string> name_set;
    for (R_xlen_t k = 0; k < names_filter.size(); k++) {
      name_set.insert(std::string(names_filter[k]));
    }
    for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(ncof); i++) {
      if (name_set.count(std::string(cof_structs[i].cofname))) {
        sel_idx.push_back(i);
      }
    }
  } else {
    sel_idx.resize(static_cast<size_t>(ncof));
    std::iota(sel_idx.begin(), sel_idx.end(), 0);
  }

  using namespace cpp11::literals;

  R_xlen_t nsel = static_cast<R_xlen_t>(sel_idx.size());
  cpp11::writable::strings cof_cofname(nsel);
  cpp11::writable::doubles cof_begadd(nsel);
  cpp11::writable::integers cof_size(nsel);
  cpp11::writable::doubles cof_matsize(nsel);
  cpp11::writable::strings cof_setid(nsel);
  cpp11::writable::strings cof_antidims(nsel);
  cpp11::writable::logicals cof_postsim(nsel);
  cpp11::writable::logicals cof_parameter(nsel);

  uvadd total_ele = 0;
  for (R_xlen_t j = 0; j < nsel; j++) {
    R_xlen_t i = sel_idx[j];
    cof_cofname[j] = std::string(cof_structs[i].cofname);
    cof_begadd[j]  = static_cast<double>(cof_structs[i].begadd);
    cof_size[j]    = cof_structs[i].size;
    cof_matsize[j] = static_cast<double>(cof_structs[i].matsize);
    int ndim = active_count(cof_structs[i].antidims);
    cof_setid[j]    = pack_n(cof_structs[i].setid,    ndim);
    cof_antidims[j] = pack_n(cof_structs[i].antidims, ndim);
    cof_postsim[j]   = (kind[i] & 1u) != 0;
    cof_parameter[j] = (kind[i] & 2u) != 0;
    total_ele += cof_structs[i].matsize;
  }

  cpp11::writable::list cof_list(
    {"cofname"_nm = (SEXP)cof_cofname,
     "begadd"_nm = (SEXP)cof_begadd,
     "size"_nm = (SEXP)cof_size,
     "setid"_nm = (SEXP)cof_setid,
     "antidims"_nm = (SEXP)cof_antidims,
     "matsize"_nm = (SEXP)cof_matsize,
     "postsim"_nm = (SEXP)cof_postsim,
     "parameter"_nm = (SEXP)cof_parameter}
  );

  cpp11::sexp bin_sexp = R_NilValue;
  if (read_values) {
    std::string bin_path = path_prefix + "cbin";
    std::ifstream bin_file(bin_path, std::ios::binary);
    if (!bin_file.is_open()) {
      cpp11::stop("Cannot open .cbin file: %s", bin_path.c_str());
    }
    cpp11::writable::doubles bin_vec(static_cast<R_xlen_t>(total_ele));
    if (filter) {
      R_xlen_t write_pos = 0;
      for (auto i : sel_idx) {
        uvadd beg = cof_structs[i].begadd;
        uvadd mat = cof_structs[i].matsize;
        bin_file.seekg(static_cast<std::streamoff>(beg) * sizeof(forreal),
                       std::ios::beg);
        bin_file.read(reinterpret_cast<char*>(REAL(bin_vec) + write_pos),
                      sizeof(forreal) * mat);
        write_pos += static_cast<R_xlen_t>(mat);
      }
    } else {
      bin_file.read(reinterpret_cast<char*>(REAL(bin_vec)),
                    sizeof(forreal) * ncofele);
    }
    if (!bin_file) {
      cpp11::stop("Truncated .cbin file: %s", bin_path.c_str());
    }
    bin_file.close();
    bin_sexp = (SEXP)bin_vec;
  }

  cpp11::writable::list result(
    {"cof"_nm = (SEXP)cof_list,
     "bin"_nm = bin_sexp}
  );
  return result;
}
