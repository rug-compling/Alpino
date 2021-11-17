data=px.gz pq.gz pc.gz pp.gz pd.gz pw.gz pi.gz

data: $(data)

install:
	cp $(data) ../

ALPINO=../../../../src/Alpino \
		print_table_total=off \
                user_max=600000\
                max_sentence_length=30\
                display_main_parts=off\
                -notk \
		end_hook=train_fluency \
		batch_command=train_generation


pq.gz:
	$(ALPINO) -flag suite ../../../../Suites/qa -flag treebank ../../../../Treebank/qa 2>pq.log | gzip > pq.gz

cgz=c_part00001.cgz c_part00002.cgz c_part00003.cgz c_part00004.cgz c_part00005.cgz c_part00006.cgz c_part00007.cgz c_part00008.cgz c_part00009.cgz c_part00010.cgz c_part00011.cgz c_part00012.cgz c_part00013.cgz c_part00014.cgz c_part00015.cgz c_part00016.cgz c_part00017.cgz c_part00018.cgz c_part00019.cgz c_part00020.cgz c_part00021.cgz c_part00022.cgz c_part00023.cgz c_part00024.cgz c_part00025.cgz c_part00026.cgz c_part00027.cgz c_part00028.cgz c_part00029.cgz c_part00030.cgz c_part00031.cgz c_part00032.cgz c_part00033.cgz c_part00034.cgz c_part00035.cgz c_part00036.cgz c_part00037.cgz c_part00038.cgz c_part00039.cgz c_part00040.cgz c_part00041.cgz c_part00042.cgz c_part00043.cgz c_part00044.cgz c_part00045.cgz c_part00046.cgz c_part00047.cgz c_part00048.cgz c_part00049.cgz c_part00050.cgz c_part00051.cgz c_part00052.cgz c_part00053.cgz c_part00054.cgz c_part00055.cgz c_part00056.cgz c_part00057.cgz c_part00058.cgz c_part00059.cgz c_part00060.cgz c_part00061.cgz c_part00062.cgz c_part00063.cgz c_part00064.cgz c_part00065.cgz c_part00066.cgz c_part00067.cgz c_part00068.cgz c_part00069.cgz c_part00070.cgz c_part00071.cgz c_part00072.cgz

pc.gz: $(cgz)
	zcat $(cgz) | gzip > pc.gz

pgz=p_part00001.pgz p_part00002.pgz p_part00003.pgz p_part00004.pgz p_part00005.pgz p_part00006.pgz p_part00007.pgz p_part00008.pgz p_part00009.pgz p_part00010.pgz p_part00011.pgz p_part00012.pgz p_part00013.pgz p_part00014.pgz p_part00015.pgz p_part00016.pgz p_part00017.pgz p_part00018.pgz p_part00019.pgz p_part00020.pgz p_part00021.pgz p_part00022.pgz p_part00023.pgz

pp.gz: $(pgz)
	zcat $(pgz) | gzip > pp.gz

dgz=d_part00001.dgz d_part00002.dgz d_part00003.dgz d_part00004.dgz d_part00005.dgz d_part00006.dgz d_part00007.dgz d_part00008.dgz d_part00009.dgz d_part00010.dgz d_part00011.dgz d_part00012.dgz d_part00013.dgz d_part00014.dgz d_part00015.dgz d_part00016.dgz d_part00017.dgz d_part00018.dgz d_part00019.dgz d_part00020.dgz d_part00021.dgz d_part00022.dgz d_part00023.dgz d_part00024.dgz d_part00025.dgz d_part00026.dgz d_part00027.dgz d_part00028.dgz d_part00029.dgz d_part00030.dgz d_part00031.dgz d_part00032.dgz d_part00033.dgz d_part00034.dgz d_part00035.dgz d_part00036.dgz d_part00037.dgz d_part00038.dgz d_part00039.dgz d_part00040.dgz d_part00041.dgz d_part00042.dgz d_part00043.dgz d_part00044.dgz d_part00045.dgz d_part00046.dgz d_part00047.dgz d_part00048.dgz d_part00049.dgz d_part00050.dgz d_part00051.dgz d_part00052.dgz d_part00053.dgz d_part00054.dgz d_part00055.dgz d_part00056.dgz d_part00057.dgz d_part00058.dgz d_part00059.dgz d_part00060.dgz d_part00061.dgz d_part00062.dgz d_part00063.dgz d_part00064.dgz d_part00065.dgz d_part00066.dgz d_part00067.dgz d_part00068.dgz d_part00069.dgz d_part00070.dgz d_part00071.dgz d_part00072.dgz d_part00073.dgz d_part00074.dgz d_part00075.dgz d_part00076.dgz d_part00077.dgz d_part00078.dgz d_part00079.dgz d_part00080.dgz d_part00081.dgz d_part00082.dgz d_part00083.dgz d_part00084.dgz d_part00085.dgz d_part00086.dgz d_part00087.dgz d_part00088.dgz d_part00089.dgz d_part00090.dgz d_part00091.dgz d_part00092.dgz d_part00093.dgz d_part00094.dgz d_part00095.dgz d_part00096.dgz d_part00097.dgz d_part00098.dgz d_part00099.dgz d_part00100.dgz d_part00101.dgz d_part00102.dgz d_part00103.dgz d_part00104.dgz d_part00105.dgz d_part00106.dgz d_part00107.dgz d_part00108.dgz d_part00109.dgz d_part00110.dgz d_part00111.dgz d_part00112.dgz d_part00113.dgz d_part00114.dgz d_part00115.dgz d_part00116.dgz d_part00117.dgz d_part00118.dgz

pd.gz: $(dgz) 
	zcat $(dgz) | gzip > pd.gz

wgz=w_part00001.wgz w_part00002.wgz w_part00003.wgz w_part00004.wgz w_part00005.wgz w_part00006.wgz w_part00007.wgz w_part00008.wgz w_part00009.wgz w_part00010.wgz w_part00011.wgz w_part00012.wgz w_part00013.wgz w_part00014.wgz w_part00015.wgz w_part00016.wgz w_part00017.wgz w_part00018.wgz w_part00019.wgz w_part00020.wgz w_part00021.wgz w_part00022.wgz w_part00023.wgz w_part00024.wgz w_part00025.wgz w_part00026.wgz w_part00027.wgz w_part00028.wgz w_part00029.wgz w_part00030.wgz w_part00031.wgz w_part00032.wgz w_part00033.wgz w_part00034.wgz w_part00035.wgz w_part00036.wgz w_part00037.wgz w_part00038.wgz w_part00039.wgz w_part00040.wgz w_part00041.wgz w_part00042.wgz w_part00043.wgz w_part00044.wgz w_part00045.wgz w_part00046.wgz w_part00047.wgz w_part00048.wgz w_part00049.wgz w_part00050.wgz w_part00051.wgz w_part00052.wgz w_part00053.wgz w_part00054.wgz w_part00055.wgz w_part00056.wgz w_part00057.wgz w_part00058.wgz w_part00059.wgz w_part00060.wgz w_part00061.wgz w_part00062.wgz w_part00063.wgz w_part00064.wgz w_part00065.wgz w_part00066.wgz w_part00068.wgz w_part00069.wgz w_part00070.wgz w_part00071.wgz w_part00072.wgz w_part00073.wgz w_part00074.wgz

pw.gz: $(wgz)
	zcat $(wgz) | gzip > pw.gz

igz=i_part00001.igz i_part00002.igz i_part00003.igz i_part00004.igz i_part00005.igz i_part00006.igz i_part00007.igz i_part00008.igz i_part00009.igz i_part00010.igz i_part00011.igz i_part00012.igz i_part00013.igz i_part00014.igz i_part00015.igz i_part00016.igz i_part00017.igz i_part00018.igz i_part00019.igz i_part00020.igz i_part00021.igz i_part00022.igz i_part00023.igz i_part00024.igz i_part00025.igz i_part00026.igz i_part00027.igz i_part00028.igz i_part00029.igz i_part00030.igz i_part00031.igz i_part00032.igz i_part00033.igz i_part00034.igz i_part00035.igz i_part00036.igz i_part00037.igz i_part00038.igz i_part00039.igz i_part00040.igz i_part00041.igz i_part00042.igz i_part00043.igz i_part00044.igz i_part00045.igz i_part00046.igz i_part00047.igz i_part00048.igz i_part00049.igz i_part00050.igz i_part00051.igz i_part00052.igz i_part00053.igz i_part00054.igz i_part00055.igz i_part00056.igz i_part00057.igz i_part00058.igz

pi.gz: $(igz)
	zcat $(igz) | gzip > pi.gz

xgz=x_part00001.xgz x_part00002.xgz x_part00003.xgz x_part00004.xgz x_part00005.xgz x_part00006.xgz x_part00007.xgz x_part00008.xgz x_part00009.xgz x_part00010.xgz x_part00011.xgz x_part00012.xgz x_part00013.xgz x_part00014.xgz x_part00015.xgz x_part00016.xgz x_part00017.xgz

px.gz: $(xgz)
	zcat $(xgz) | gzip > px.gz

ugz=u_part00001.ugz u_part00002.ugz u_part00003.ugz u_part00004.ugz u_part00005.ugz u_part00006.ugz u_part00007.ugz u_part00008.ugz u_part00009.ugz u_part00010.ugz u_part00011.ugz u_part00012.ugz u_part00013.ugz u_part00014.ugz u_part00015.ugz u_part00016.ugz u_part00017.ugz u_part00018.ugz u_part00019.ugz u_part00020.ugz u_part00021.ugz u_part00022.ugz u_part00023.ugz u_part00024.ugz u_part00025.ugz u_part00026.ugz u_part00027.ugz u_part00028.ugz u_part00029.ugz u_part00030.ugz u_part00031.ugz u_part00032.ugz u_part00033.ugz u_part00034.ugz u_part00035.ugz u_part00036.ugz u_part00037.ugz u_part00038.ugz u_part00039.ugz u_part00040.ugz u_part00041.ugz u_part00042.ugz u_part00043.ugz u_part00044.ugz u_part00045.ugz u_part00046.ugz u_part00047.ugz u_part00048.ugz u_part00049.ugz u_part00050.ugz u_part00051.ugz u_part00052.ugz u_part00053.ugz u_part00054.ugz u_part00055.ugz u_part00056.ugz u_part00057.ugz u_part00058.ugz u_part00059.ugz u_part00060.ugz u_part00061.ugz u_part00062.ugz u_part00063.ugz u_part00064.ugz u_part00065.ugz u_part00066.ugz u_part00067.ugz u_part00068.ugz u_part00069.ugz u_part00070.ugz u_part00071.ugz u_part00072.ugz u_part00073.ugz u_part00074.ugz u_part00075.ugz u_part00076.ugz u_part00077.ugz u_part00078.ugz u_part00079.ugz u_part00080.ugz u_part00081.ugz u_part00082.ugz u_part00083.ugz u_part00084.ugz u_part00085.ugz u_part00086.ugz u_part00087.ugz u_part00088.ugz u_part00089.ugz u_part00090.ugz u_part00091.ugz u_part00092.ugz u_part00093.ugz u_part00094.ugz u_part00095.ugz u_part00096.ugz u_part00097.ugz u_part00098.ugz u_part00099.ugz u_part00100.ugz u_part00101.ugz u_part00102.ugz u_part00103.ugz u_part00104.ugz u_part00105.ugz u_part00106.ugz u_part00107.ugz u_part00108.ugz u_part00109.ugz u_part00110.ugz u_part00111.ugz u_part00112.ugz u_part00113.ugz u_part00114.ugz u_part00115.ugz u_part00116.ugz u_part00117.ugz u_part00118.ugz u_part00119.ugz u_part00120.ugz u_part00121.ugz u_part00122.ugz u_part00123.ugz u_part00124.ugz u_part00125.ugz u_part00126.ugz u_part00127.ugz u_part00128.ugz u_part00129.ugz u_part00130.ugz u_part00131.ugz u_part00132.ugz u_part00133.ugz u_part00134.ugz u_part00135.ugz u_part00136.ugz u_part00137.ugz u_part00138.ugz u_part00139.ugz u_part00140.ugz u_part00141.ugz

pu.gz: $(ugz)
	zcat $(ugz) | gzip > pu.gz

%.xgz: ../../../../Suites/X/%.pl
	$(ALPINO) -flag suite $< -flag treebank ../../../../Treebank/extra 2>$*.log | gzip > $*.xgz

%.dgz: ../../../../Suites/D/%.pl
	$(ALPINO) -flag suite $< -flag treebank ../../../../Treebank/dpc 2>$*.log | gzip > $*.dgz

%.pgz: ../../../../Suites/P/%.pl
	$(ALPINO) -flag suite $< -flag treebank ../../../../Treebank/PPH 2>$*.log | gzip > $*.pgz

%.wgz: ../../../../Suites/W/%.pl
	$(ALPINO) -flag suite $< -flag treebank ../../../../Treebank/WIKI 2>$*.log | gzip > $*.wgz

%.cgz: ../../../../Suites/C/%.pl
	$(ALPINO) -flag suite $< -flag treebank ../../../../Treebank/cdb 2>$*.log | gzip > $*.cgz

%.igz: ../../../../Suites/I/%.pl
	$(ALPINO) -flag suite $< -flag treebank ../../../../Treebank/PPI 2>$*.log | gzip > $*.igz

%.ugz: ../../../../Suites/U/%.pl
	$(ALPINO) -flag suite $< -flag treebank ../../../../Treebank/WSU 2>$*.log | gzip > $*.ugz

