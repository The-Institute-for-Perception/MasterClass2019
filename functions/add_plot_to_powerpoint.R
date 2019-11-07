### this function adds ggplots to a powerpoint slide

add_plot_to_powerpoint <- function(plot, path){
   if(!file.exists(path)) {
      out <- officer::read_pptx()
   } else {
      out <- officer::read_pptx(path)
   }
   
   out %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      rvg::ph_with_vg(code = print(plot), type = "body") %>% 
      print(target = path)
}
