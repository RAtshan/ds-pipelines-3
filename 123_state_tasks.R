do_state_tasks <- function(oldest_active_sites, ...) {

  # Define task table rows
  # TODO: DEFINE A VECTOR OF TASK NAMES HERE
  task_names <- oldest_active_sites$state_cd

  # Define task table columns
  download_step <- create_task_step(
    step_name = 'download',
    # TODO: Make target names like "WI_data"
    target_name = function(task_name, step_name, ...){
      sprintf('%s_data', task_name)
    },
    # TODO: Make commands that call get_site_data()
   command = function(task_name, ...){
     sprintf("get_site_data(sites_info = oldest_active_sites, state = I('%s'), parameter = parameter)", task_name)
   }
)

  # Create the task_plan
  task_plan <- create_task_plan(
    task_names = task_names ,
    task_steps = list(download_step),
    add_complete = FALSE)

  # Create the task remakefile
  create_task_makefile(
    # TODO: ADD ARGUMENTS HERE
    task_plan = task_plan,
    makefile = '123_state_tasks.yml',
    include = 'remake.yml',
    packages = c('tidyverse', 'dataRetrieval'),
    sources = "'1_fetch/src/get_site_data.R'",
    tickquote_combinee_objects = FALSE,
    finalize_funs = c())

  # Build the tasks
  scmake('123_state_tasks', remake_file='123_state_tasks.yml')
  # Return nothing to the parent remake file
  return()
}
