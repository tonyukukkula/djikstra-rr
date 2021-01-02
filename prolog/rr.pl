ana :-
    schedule([1-2, 1-3, 1-4, ... 2-1, 2-3, ...], [], SCHEDULE)
    cikti(SCHEDULE).

schedule([], SCHEDULE, SCHEDULE).
schedule(ISLEMLER, PARTIAL_SCHEDULE, SCHEDULE) :-
  sec(ISLEMLER, ISLEM_ZAMANLARI, KALAN_ISLEMLER),
  test(ISLEM_ZAMANLARI, PARTIAL_SCHEDULE),
  schedule(KALAN_ISLEMLER, [ISLEM_ZAMANLARI|PARTIAL_SCHEDULE], SCHEDULE).

schedule(SCHEDULE) :-
    islemler(ISLEMLER),
    ilk_kisim(ISLEMLER, [], SCHEDULE_1, [], ISLEMLER_2),
    ikinci_kisim(ISLEMLER_2, SCHEDULE_1, SCHEDULE).

al_islem([], _).
al_islem([ISLEME_ALINAN_ISLEMLER|ISLEMLER], SECME_LISTESI) :-
  al(ISLEME_ALINAN_ISLEMLER, SECME_LISTESI, SECME_LISTESI_KALAN),
  temizle(ISLEME_ALINAN_ISLEMLER, SECME_LISTESI_KALAN, SECME_LISTESI_TEMIZLENEN),
  al_islem(ISLEMLER, SECME_LISTESI_TEMIZLENEN).

sil(A,[A|X],X).
sil(A,[B|X],[B|Y]) :- sil(A,X,Y).

sec(A,[A|X],X).
sec(A,[B|X],Y) :- sec(A,X,Y).

sira_uyesi(H, [H|T]).
sira_uyesi(H, [X|T]) :- sira_uyesi(H, T).

sira_uyesi(H, [H|_]).
sira_uyesi(H, [_|T]) :- sira_uyesi(H, T).

terscevir([], R, R).
terscevir([H|T], PARTIAL, R) :-
    terscevir(T, [H|PARTIAL], R).
