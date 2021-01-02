def WT(islemler, islem_tane, islem_ates_zaman, islem_bekleme_zaman, islem_kesit)
    islemler_ates_zaman_kopya= Marshal.load(Marshal.dump(islem_ates_zaman))
    t=0
    unless false
      done=true
      for i in 0...islem_tane
        if islemler_ates_zaman_kopya[i] >0
          done=false
          if islemler_ates_zaman_kopya[i] > islem_kesit
            t= t.to_i + islem_kesit
            islemler_ates_zaman_kopya[i] -= islem_kesit
          else
            t= t.to_i+ islemler_ates_zaman_kopya[i]
            islem_bekleme_zaman[i]= t.to_i - islem_bekleme_zaman[i].to_i
            islem_bekleme_zaman[i]=0
          end        
        end
      end
      if done==true
        return
      end
    end
  end
  
  def TAT(islemler, islem_tane, islem_ates_zaman, islem_bekleme_zaman, islem_kesit, islem_tat)
    for i in 0...islem_tane
      islem_tat[i]=islem_ates_zaman[i].to_i+islem_bekleme_zaman[i].to_i
    end
  end
  
  def rr(islemler, islem_tane, islem_ates_zaman, islem_kesit)
    islem_bekleme_zaman= Array.new(islem_tane)
    islem_tat= Array.new(islem_tane)
    total_wt=0
    total_tat=0
    WT(islemler, islem_tane, islem_ates_zaman, islem_bekleme_zaman, islem_kesit)
    TAT(islemler, islem_tane, islem_ates_zaman, islem_bekleme_zaman, islem_kesit, islem_tat)
    print "islemler | Burst Time | Waiting Time | Turn-around Time"
    toplam_beklemesuresi = 0
    toplam_geridonussuresi = 0
  
    for i in 0...islem_tane
      toplam_beklemesuresi = toplam_beklemesuresi.to_i + islem_bekleme_zaman[i].to_i
      toplam_geridonussuresi = toplam_geridonussuresi.to_i + islem_tat[i].to_i
      print("\n ", i+1 , "\t\t" , islem_ates_zaman[i] , "\t\t", islem_bekleme_zaman[i] ,"\t\t", islem_tat[i] )
    end
    printf("\nOrtalama bekleme suresi : %.5f" ,(toplam_beklemesuresi/islem_tane))
    printf("\nOrtalama geri donus suresi : %.5f", (toplam_geridonussuresi/islem_tane))
  end
  
  islemler=Array.[](1,2,3)
  islem_tane=3
  islem_ates_zaman= Array.[](10,5,8)
  islem_kesit=2
  rr(islemler, islem_tane, islem_ates_zaman,islem_kesit)
  
