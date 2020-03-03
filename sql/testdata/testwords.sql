INSERT INTO language_tbl(id, version, name, createdby, createdwhen, modiby, modiwhen) VALUES
 (1, 1, 'Proto-Material', 'wiki', now(), 'wiki', now()),
 (2, 1, 'Proto-Monster', 'wiki', now(), 'wiki', now());

INSERT INTO word_tbl(id, version, word, lang_id, createdby, createdwhen, modiby, modiwhen) VALUES
 (1, 1, 'H2enH1', 1, 'wiki', now(), 'wiki', now()),
 (2, 1, 'ĝonH2dʰos', 1, 'wiki', now(), 'wiki', now());


 nextval('pk_sequence')