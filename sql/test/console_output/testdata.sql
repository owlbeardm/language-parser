-- test data for console output

delete from evolve_law_tbl       ;
delete from translation_tbl      ;
delete from word_origin_from_tbl ;
delete from word_origin_tbl      ;
delete from word_tbl             ;
delete from language_tbl         ;

INSERT INTO language_tbl(id, name) VALUES
 (1, 'ProtoMonster'), 
 (2, 'ProtoDragon'),
 (3, 'Dragon'), 
 (4, 'English');

INSERT INTO word_tbl(id, word, part_of_speech, lang_id) VALUES
(1, 'село', 'Noun', 1),   -- village (technically, a village is a село)
                          -- hamlet (whereas a деревня is a hamlet)
                          -- rural areas (напри. вливание в развитие села — injections into rural areas aza)
(2, 'село', 'Verb', 1);   -- sit
                          -- sit up (из лежачего положения)
                          -- mount (на коня, в боевую машину, на "броню")

INSERT INTO word_tbl(id, word, part_of_speech, lang_id) VALUES
(101, 'village', 'Noun', 4),   
(102, 'hamlet', 'Noun', 4),   
(103, 'sit', 'Verb', 4),   
(104, 'sit up', 'Verb', 4),   
(105, 'mount', 'Verb', 4);  


INSERT INTO translation_tbl(id, from_word_id, to_lang_id, to_word_id, comment, alt_translation) VALUES
(1, 1, 4, 101, 'technically, a village is a село', null),
(2, 1, 4, 102, 'whereas a деревня is a hamlet', null),
(3, 1, 4, null, 'напри. вливание в развитие села — injections into rural areas aza', 'rural areas'),
(4, 2, 4, 103, null, null),
(5, 2, 4, 104, 'из лежачего положения', null),
(6, 2, 4, 105, 'на коня, в боевую машину, на "броню"', null);