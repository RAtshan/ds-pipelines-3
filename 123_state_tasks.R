do_state_tasks <- function(oldest_active_sites, ...) {

  split_inventory(summary_file = '1_fetch/tmp/state_splits.yml', oldest_active_sites)
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
     sprintf("get_site_data(sites_info_file = '1_fetch/tmp/inventory_%s.tsv', parameter = parameter)", task_name)
   }
)
  # New step to create plots
  plot_step <- create_task_step(
    step_name = 'plot',
    target_name = function(task_name, step_name,  ...){
      sprintf("3_visualize/out/timeseries_%s.png", task_name)
    },
    command = function(steps, ...){
      sprintf("plot_site_data(out_file = target_name, site_data = %s, parameter=parameter)", steps[['download']]$target_name)
    }
)
  # New Step "tally_step"
  tally_step <- create_task_step(
    step_name = 'tally',
    target_name = function(task_name, step_name, ...){
      sprintf("%s_tally", task_name)
      },
    command = function(steps, ...){
      sprintf("tally_site_obs(site_data = %s)", steps[['download']]$target_name)
    }
  )
  # Create the task_plan
  task_plan <- create_task_plan(
    task_names = task_names ,
    task_steps = list(download_step, plot_step, tally_step),
    add_complete = FALSE)

  # Create the task remakefile
  create_task_makefile(
    # TODO: ADD ARGUMENTS HERE
    task_plan = task_plan,
    makefile = '123_state_tasks.yml',
    include = 'remake.yml',
    packages = c('tidyverse', 'dataRetrieval' , 'lubridate'),
    sources = c('1_fetch/src/get_site_data.R', '3_visualize/src/plot_site_data.R',  '2_process/src/tally_site_obs.R'),
    tickquote_combinee_objects = FALSE,
    finalize_funs = c())

  # Build the tasks
  scmake('123_state_tasks', remake_file='123_state_tasks.yml')
  # Return nothing to the parent remake file
  return()
}

# Issue #6: Creating a splitter so each task target retrieve and build the needed State's data.

split_inventory <- function(
  summary_file = '1_fetch/tmp/state_splits.yml',
  sites_info = oldest_active_sites){
  if(!dir.exists('1_fetch/temp')) dir.create('1_fetch/tmp')
  sort_split_inventory = c()
    #vector(mode = "list", length = nrow(sites_info))
  #creating a loop to over  the rows in oldest_active_sites and save each row to a file
  for(i in 1:nrow(sites_info)){
    state_abb = sites_info[i, 'state_cd']
    inventory_name = sprintf("1_fetch/tmp/inventory_%s.tsv", state_abb)
    readr::write_tsv(sites_info[i, ], path = inventory_name)
    sort_split_inventory =  append(sort_split_inventory, inventory_name)
}
  # to sort the above tsv files in alphabetical order.
  sort_split_inventory = sort(sort_split_inventory)
  # to write a summary file (yml in this case) using the above file path.
  scipiper::sc_indicate(ind_file = summary_file, data_file = sort_split_inventory)
}
