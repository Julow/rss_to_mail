let () =
  let mdx = Mdx_top.init ~verbose:false ~silent:false ~verbose_findlib:false () in
  Mdx_test.run mdx ~force_output:true ~output:`Stdout Sys.argv.(1)
