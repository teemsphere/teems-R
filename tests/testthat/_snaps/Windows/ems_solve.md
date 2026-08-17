# ems_solve errors when solution singularity detected

    Code
      ems_solve(cmf_path)
    Condition
      Error in `ems_solve()`:
      x Singularity detected during solution. See 'C:/Users/PC/AppData/Local/R/cache/R/teems/solve/solve_err_sing/out/solver_out_HHMM.txt'.

# ems_solve informs terminal run

    Code
      ems_solve(cmf_path, terminal_run = TRUE)
    Message
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=C:/Users/PC/AppData/Local/R/cache/R/teems/solve/solve_info_terminal,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/hsl -cmdfile /opt/teems/GTAPv7.cmf -matsol 0  -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200 -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant hsl process early according to your OS-specific
      system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      'C:/Users/PC/AppData/Local/R/cache/R/teems/solve/solve_info_terminal/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("C:/Users/PC/AppData/Local/R/cache/R/teems/solve/solve_info_terminal/GTAPv7.cmf")`

