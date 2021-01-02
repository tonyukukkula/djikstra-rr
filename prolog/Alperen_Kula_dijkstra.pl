%perl scriptimizi tanımlayalım.
dijkstra(Vertex, Ss):-
    olustur(Vertex, [Vertex], Ds),
    subdijkstra(Ds, [s(Vertex,0,[])], Ss). %s==> structure
%sayın hocam yazacağınız querylerle oluşturduğum şehirler grafından gerçek sorgulama yapabilirsiniz. gerçek ölçüleri kullandım,
%şehir dışına sıkça çıktığınızı düşünerek belki de çokca çıkmıyorsunuzdur, bilemedim.

%SAYIN HOCAM TÜM ŞEHİRLERİ EKLEMEDİM TABİİ, LÜTFEN ŞEHİR LİSTESİNİ KONTROL EDİNİZ.%

%başlangıçta factleri tanımlamakta yarar var. sonrasında rule ları yazarık.
best([], Best, Best). 
subdijkstra([], Ss, Ss).
sil([], _, []).
artttttir([], _, []).  
bol([], Ys, Ys). 
eq(s(X,_,_), s(X,_,_)).  
olustur(_, _, []).
member(X, [X|_]).
terscevir_1([], As, As).

%şimdi rule ları yazalım.
%okunabilirliği arttıracak şekilde boşluk,satır aralığı(enter) ve girintileme yaptım.
shorter(s(_,X,_), s(_,Y,_)):-X < Y.

lt(s(X,_,_), s(Y,_,_)):-X @< Y.

subdijkstra([D|Ds], Ss0, Ss):-
    best(Ds, D, S),
    sil([D|Ds], [S], Ds1),
    S=s(Vertex,Distance,Path),%s==> structure
    terscevir([Vertex|Path], Path1),
    bol(Ss0, [s(Vertex,Distance,Path1)], Ss1),
    olustur(Vertex, [Vertex|Path], Ds2),
    sil(Ds2, Ss1, Ds3),
    artttttir(Ds3, Distance, Ds4),
    bol(Ds1, Ds4, Ds5),
    subdijkstra(Ds5, Ss1, Ss).

path(Vertex0, Vertex, Path, Dist):-
    dijkstra(Vertex0, Ss),
    member(s(Vertex,Dist,Path), Ss), !.
    
olustur(Start, Path, Edges):-
    setof(s(Vertex,Edge,Path), e(Start,Vertex,Edge), Edges), !.

best([Edge|Edges], Best0, Best):-
    shorter(Edge, Best0), !,
    best(Edges, Edge, Best).

best([_|Edges], Best0, Best):-
    best(Edges, Best0, Best).
 
sil([X|Xs], [], [X|Xs]):-!. 

sil([X|Xs], [Y|Ys], Ds):-
    eq(X, Y), !, 
    sil(Xs, Ys, Ds). 

sil([X|Xs], [Y|Ys], [X|Ds]):-
    lt(X, Y), !, sil(Xs, [Y|Ys], Ds). 

sil([X|Xs], [_|Ys], Ds):-
    sil([X|Xs], Ys, Ds). 

bol([X|Xs], [], [X|Xs]):-!. 

bol([X|Xs], [Y|Ys], [X|Zs]):-
    eq(X, Y), shorter(X, Y), !, 
    bol(Xs, Ys, Zs).

bol([X|Xs], [Y|Ys], [Y|Zs]):-
    eq(X, Y), !, 
    bol(Xs, Ys, Zs).

bol([X|Xs], [Y|Ys], [X|Zs]):-
    lt(X, Y), !, 
    bol(Xs, [Y|Ys], Zs).

bol([X|Xs], [Y|Ys], [Y|Zs]):-
    bol([X|Xs], Ys, Zs).

artttttir([s(V,D1,P)|Xs], Incr, [s(V,D2,P)|Ys]):-
    D2 is D1 + Incr,
    artttttir(Xs, Incr, Ys).

member(X, [_|Ys]):-member(X, Ys).

terscevir(Xs, Ys):-terscevir_1(Xs, [], Ys).
  
terscevir_1([X|Xs], As, Ys):-terscevir_1(Xs, [X|As], Ys).
  
e(X, Y, Z):-mesafe(X, Y, Z).
e(X, Y, Z):-mesafe(Y, X, Z).
  
% NIGDE <3 %
mesafe(nigde,       istanbul,     797).
mesafe(nigde,       bolu,         535).
mesafe(nigde,       canakkale,    983).
mesafe(nigde,       cankiri,      387).
mesafe(nigde,       mersin,       198).
mesafe(nigde,       corum,        370).
mesafe(nigde,       antalya,      544).
mesafe(nigde,       sivas,        323).
mesafe(nigde,       gayseri,      128).
mesafe(gonya,       edirne,       890).
mesafe(gonya,       kutahya,      323).
mesafe(angara,      nigde,        348).
mesafe(angara,      canakkale,    653).
mesafe(angara,      cankiri,      131).
mesafe(angara,      corum,        242).
mesafe(angara,      sivas,        440).
mesafe(angara,      gayseri,      318).
mesafe(erzincan,    istanbul,     1036).
mesafe(erzincan,    agri,         370).
mesafe(erzincan,    antalya,      1056).
mesafe(erzincan,    afyon,        938).
mesafe(erzincan,    yozgat,       470).
mesafe(istanbul,    balikesir,    390).
mesafe(istanbul,    antalya,      716).
mesafe(istanbul,    afyon,        452).
mesafe(istanbul,    gayseri,      771).
mesafe(istanbul,    yozgat,       669).
mesafe(bolu,        corum,        352).
mesafe(bolu,        antalya,      682).
mesafe(bolu,        yozgat,       407).