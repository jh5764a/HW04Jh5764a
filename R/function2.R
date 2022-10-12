

#' Function 2 creates plots based on the class input in the function
#'
#' @param dataset is a data frame of the users choosing
#' @param class_name
#'
#' @return returns a histogram of the variables that are of class class_name if the class is numeric, double, or integer, returns a bar plot of the variables that are of class class_name if the class is factor, logical, or character
#' @export
#'
#' @examples
#' A <- c("J","e","d","H","a","m","m","o","u","d")
#' B <- c(1,2,3,4,1,2,3,4,4,6)
#' C <- c(1,2,3,2,1,2,1,2,3,3)
#' D <- c("a","b","c","c","d","d","e","f","g","f")
#' E <- c(1,4,5,6,7,4,5,42,8,11)
#' f <-c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE)
#' Example1 <- data.frame(A,B,C,D,E,f)
#' A1 <- c("g","f","d","d","l","n","z","w","ud","d")
#' B2 <- c(11,22,33,44,11,22,33,44,44,66)
#' C3 <- c(1,4,9,4,1,4,1,8,33,1089)
#' D4 <- c("a","b","c","c","d","d","e","f","g","f")
#' E5 <- c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE)
#' f6 <-c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)
#' Example2 <- data.frame(A1,B2,C3,D4,E5,f6)
#' examp1A <- Function_2(Example1, "numeric")
#' examp1A
#' examp2A <- Function_2(Example2, "logical")
#' examp2A







Function_2 <- function(dataset, class_name){
  class_data <- c(sapply(dataset,class))

  if(!(class_name %in% class_data))
    stop("the class name you've chosen does not exist in this dataset")

  if (class_name == "numeric"){
    dataset1 <- dplyr::select(dataset, tidyselect::vars_select_helpers$where(is.numeric))

    if (length(dataset1 > 0)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot2::ggplot(dataset1, ggplot2::aes(x = values))+
        ggplot2::geom_histogram()+
        ggplot2::ggtitle(paste0("Variables in dataset of class ", class_name))+
        ggplot2::facet_wrap(var ~.)->Graph1

    }

    else {
      print("there are no class numerics in this dataset.")
    }
  }

  else if (class_name == "double"){
    dataset1 <- dplyr::select(dataset, tidyselect::vars_select_helpers$where(is.double))

    if (length(dataset1 > 0)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot2::ggplot(dataset1, ggplot2::aes(x = values))+
        ggplot2::geom_histogram()+
        ggplot2::ggtitle(paste0("Variables in dataset of class ", class_name))+
        ggplot2::facet_wrap(var ~.)->Graph1

    }

    else {
      print("there are no class double in this dataset.")
    }
  }
  else if (class_name == "integer"){
    dataset1 <- dplyr::select(dataset, tidyselect::vars_select_helpers$where(is.integer))

    if (length(dataset1 > 0)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot2::ggplot(dataset1, ggplot2::aes(x = values))+
        ggplot2::geom_histogram()+
        ggplot2::ggtitle(paste0("Variables in dataset of class ", class_name))+
        ggplot2::facet_wrap(var ~.)->Graph1

    }

    else {
      print("there are no class integer in this dataset.")
    }
  }
  else if (class_name == "character"){
    dataset1 <- dplyr::select(dataset, tidyselect::vars_select_helpers$where(is.character))

    if (length(dataset1 > 0)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot2::ggplot(dataset1, ggplot2::aes(x = values))+
        ggplot2::geom_bar()+
        ggplot2::ggtitle(paste0("Variables in dataset of class ", class_name))+
        ggplot2::facet_wrap(var ~.)->Graph1

    }
    else if (length(dataset1 > 7)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot2::ggplot(dataset1, ggplot2::aes(x = values))+
        ggplot2::geom_bar()+
        ggplot2::ggtitle(paste0("Variables in dataset of class ", class_name))+
        ggplot2::facet_wrap(var ~.)+
        ggplot2::coord_flip()->Graph1

    }
    else {
      print("there are no class character in this dataset.")
    }
  }
  else if (class_name == "factor"){
    dataset1 <- dplyr::select(dataset, tidyselect::vars_select_helpers$where(is.factor))

    if (length(dataset1 <= 7) && length(dataset1 > 0)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot2::ggplot(dataset1, ggplot2::aes(x = values))+
        ggplot2::geom_bar()+
        ggplot2::ggtitle(paste0("Variables in dataset of class ", class_name))+
        ggplot2::facet_wrap(var ~.)->Graph1

    }
    else if (length(dataset1 > 7)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot2::ggplot(dataset1, ggplot2::aes(x = values))+
        ggplot2::geom_bar()+
        ggplot2::ggtitle(paste0("Variables in dataset of class ", class_name))+
        ggplot2::facet_wrap(var ~.)+
        ggplot2::coord_flip()->Graph1

    }
    else {
      print("there are no class factor in this dataset.")
    }
  }
  else if (class_name == "logical"){
    dataset1 <- dplyr::select(dataset, tidyselect::vars_select_helpers$where(is.logical))

    if (length(dataset1 <= 7) && length(dataset1 > 0)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot2::ggplot(dataset1, ggplot2::aes(x = values))+
        ggplot2::geom_bar()+
        ggplot2::ggtitle(paste0("Variables in dataset of class ", class_name))+
        ggplot2::facet_wrap(var ~.)->Graph1

    }
    else if (length(dataset1 > 7)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot2::ggplot(dataset1, ggplot2::aes(x = values))+
        ggplot2::geom_bar()+
        ggplot2::ggtitle(paste0("Variables in dataset of class ", class_name))+
        ggplot2::facet_wrap(var ~.)+
        ggplot2::coord_flip()->Graph1

    }
    else {
      print("there are no class logical in this dataset.")
    }

  }
  Graph1
}


