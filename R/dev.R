





p <- function(){

  print(rlang::fn_n)

}

setInitiationInfo <- function(object, additional_input = list()){

  ce <- rlang::caller_env()

  init_fn <- rlang::caller_fn()

  init_frame <- base::sys.parent()

  init_call <- base::sys.call(which = init_frame)

  init_fn_name <- base::as.character(init_call)[1]

  init_args <-
    rlang::fn_fmls_names(fn = init_fn)

  init_args <- init_args[init_args != "..."]

  init_args_input <-
    purrr::map(
      .x = init_args,
      .f = function(arg){

        base::parse(text = arg) %>%
          base::eval(envir = ce)

      }
    ) %>%
    purrr::set_names(nm = init_args)

  init_args_input <-
    init_args_input[!base::names(init_args_input) %in% c("cds",  "coords_df", "count_mtr", "expr_mtr", "object", "seurat_object")]

  init_args_input <-
    c(init_args_input, additional_input)

  initiation_list <- list(
    init_fn = init_fn_name,
    input = init_args_input,
    time = base::Sys.time()
  )

  object@information$initiation <- initiation_list

  return(object)

}
