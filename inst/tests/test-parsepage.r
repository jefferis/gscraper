context("page parsing functions")

test_that('remotepath works',{
      testhtml=file.path('data','test.html')
      t=readLines(testhtml)
      
      
      baseline=c("http://www.nchc.org.tw/", "http://flycircuit.tw//modules.php?name=clearpage&op=lsm_movie&neuron=ChaMARCM-F001159_seg001&gender=female&id=Cha-F-000239", 
          "http://flycircuit.tw/flycircuitImage/tracing/ChaMARCM-F001159_seg001_lineset.am.gz", 
          "http://flycircuit.tw/flycircuitImage/LSM_uploadw/ChaMARCM-F001159_seg001_lsm.jpg", 
          "http://flycircuit.tw//modules.php?name=clearpage&op=lsm_movie&neuron=ChaMARCM-F001159_seg001&gender=female&id=Cha-F-000239", 
          "http://flycircuit.tw/flycircuitImage/neuron/ChaMARCM-F001159_seg001-5.jpg", 
          "http://flycircuit.tw/flycircuitImage/neuron/ChaMARCM-F001159_seg001-soma_1.jpg", 
          "http://flycircuit.tw//modules.php?name=clearpage&op=detail_table&gene_name=DvGlutMARCM-F002451_seg001&idid=11916", 
          "http://flycircuit.tw/flycircuitImage/neuron/DvGlutMARCM-F002451_seg001-5.jpg", 
          "http://flycircuit.tw//modules.php?name=clearpage&op=detail_table&gene_name=ChaMARCM-F000671_seg001&idid=5278", 
          "http://flycircuit.tw/flycircuitImage/neuron/ChaMARCM-F000671_seg001-5.jpg", 
          "http://flycircuit.tw//modules.php?name=clearpage&op=detail_table&gene_name=GadMARCM-F000418_seg001&idid=9535", 
          "http://flycircuit.tw/flycircuitImage/neuron/GadMARCM-F000418_seg001-5.jpg", 
          "http://flycircuit.tw//modules.php?name=clearpage&op=detail_table&gene_name=DvGlutMARCM-F1705-repeat0904-02_seg2&idid=10238", 
          "http://flycircuit.tw/flycircuitImage/neuron/DvGlutMARCM-F1705-repeat0904-02_seg2-5.jpg"
      )
      expect_equal(extract_links(t,rooturl='http://flycircuit.tw'),
          baseline,label = 'all links, absolute paths, no trailing slash in root url')
      
      expect_equal(extract_links(t,rooturl='http://flycircuit.tw/'),
          baseline,label = 'all links, absolute paths')
      
      baseline2=grep('flycircuitImage',baseline,value = TRUE)
      
      expect_equal(extract_links(t,rooturl='http://flycircuit.tw',regex='flycircuitImage'),
          baseline2,label = 'filter extracted links by regex')
    })
