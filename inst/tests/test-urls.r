context("url functions")

test_that('remotepath works',{
      url="http://cran.r-project.org/web/packages/scrapeR/scrapeR.pdf"
      urldoubleslash="http://cran.r-project.org//web/packages/scrapeR/scrapeR.pdf"
      file='web/packages/scrapeR/scrapeR.pdf'
      expect_equal(remotepath(url),file)
      expect_equal(remotepath(urldoubleslash),file)
      
      urls=c(url,"http://cran.r-project.org/web/packages/scrapeR/scrapeR.pdf")
      expect_equal(remotepath(urls),rep(file,2))
      
      query_url="http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&gene_name=ChaMARCM-F001159_seg001&idid=5100"
      phpq='modules.php?name=clearpage&op=detail_table&gene_name=ChaMARCM-F001159_seg001&idid=5100'
      expect_equal(remotepath(query_url),phpq)
    })
