# ems_solve errors when solution errors detected

    Code
      ems_solve(cmf_path)
    Message
      i `matrix_method` "auto": using "LU" for this static model.
    Condition
      Error in `ems_solve()`:
      x Errors detected during solution. See '/home/mpc/.cache/R/teems/solve/solve_err_error/out/solver_out_HHMM.txt'.

# ems_solve errors when solution singularity detected

    Code
      ems_solve(cmf_path)
    Message
      i `matrix_method` "auto": using "LU" for this static model.
    Condition
      Error in `ems_solve()`:
      x Singularity detected during solution. See '/home/mpc/.cache/R/teems/solve/solve_err_sing/out/solver_out_HHMM.txt'.

# ems_solve informs terminal run

    Code
      ems_solve(cmf_path, terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "LU" for this static model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=/home/mpc/.cache/R/teems/solve/solve_info_terminal,dst=/opt/teems teems:latest /bin/bash -c "/opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 0    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '/home/mpc/.cache/R/teems/solve/solve_info_terminal/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("/home/mpc/.cache/R/teems/solve/solve_info_terminal/GTAPv7.cmf")`

# matrix_method auto resolves by model type

    Code
      ems_solve(cmf_path, matrix_method = "auto", terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "LU" for this static model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=/home/mpc/.cache/R/teems/solve/solve_auto_static,dst=/opt/teems teems:latest /bin/bash -c "/opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 0    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '/home/mpc/.cache/R/teems/solve/solve_auto_static/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("/home/mpc/.cache/R/teems/solve/solve_auto_static/GTAPv7.cmf")`

---

    Code
      ems_solve(cmf_path, solution_method = "Gragg", matrix_method = "auto",
        terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "SBBD" for this intertemporal model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=/home/mpc/.cache/R/teems/solve/solve_auto_dynamic,dst=/opt/teems teems:latest /bin/bash -c "/opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAP-RE.cmf -matsol 1 -step1 2 -step2 4 -step3 8   -nsubints 1 -solmed Gragg -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '/home/mpc/.cache/R/teems/solve/solve_auto_dynamic/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("/home/mpc/.cache/R/teems/solve/solve_auto_dynamic/GTAP-RE.cmf")`

# matrix_method auto selects DBBD for large static deployments

    Code
      ems_solve(cmf_path, n_tasks = 2L, terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "DBBD" for this static model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=/home/mpc/.cache/R/teems/solve/solve_auto_dbbd,dst=/opt/teems teems:latest /bin/bash -c "/opt/teems-solver/lib/mpi/bin/mpiexec -n 2 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 2    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '/home/mpc/.cache/R/teems/solve/solve_auto_dbbd/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("/home/mpc/.cache/R/teems/solve/solve_auto_dbbd/GTAPv7.cmf")`

---

    Code
      ems_solve(cmf_path, terminal_run = TRUE)
    Message
      i This static model's size favors "DBBD": `n_tasks = 4` with `matrix_method` "DBBD" (or "auto") may solve faster than single-task "LU".
      i `matrix_method` "auto": using "LU" for this static model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=/home/mpc/.cache/R/teems/solve/solve_auto_dbbd,dst=/opt/teems teems:latest /bin/bash -c "/opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 0    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '/home/mpc/.cache/R/teems/solve/solve_auto_dbbd/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("/home/mpc/.cache/R/teems/solve/solve_auto_dbbd/GTAPv7.cmf")`

