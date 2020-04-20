delete from evolve_law_tbl       ;
delete from translation_tbl      ;
delete from word_origin_from_tbl ;
delete from word_origin_tbl      ;
delete from word_tbl             ;
delete from language_tbl         ;


INSERT INTO language_tbl(id, name) VALUES
 (1, 'ProtoMaterial'), 
 (2, 'ProtoMonster'), 
 (3, 'ProtoDragon'),
 (4, 'Dragon'), 
 (5, 'English');

INSERT INTO word_tbl(id, word, part_of_speech, lang_id) VALUES
(1, 'apó', 'Adverb', 1),   
(2, 'déiwos', 'Noun', 1),   
(3, 'doru', 'Noun', 1),   
(4, 'dreu', 'Noun', 1),   
(5, 'dyēus', 'Noun', 1),   
(6, 'egH2', 'Pronoun', 1),   
(7, 'en', 'Preposition', 1),   
(8, 'H1dn̥t', 'Noun', 1),
(9, 'kʷe', 'Conjunction', 1),
(10, 'sekʷ', 'Verb', 1); 

INSERT INTO word_tbl(id, word, part_of_speech, lang_id) VALUES
(101, 'away', 'Adverb', 5),   
(102, 'day', 'Noun', 5),   
(103, 'tree', 'Noun', 5),   
(104, 'wood', 'Noun', 5),   
(105, 'god', 'Noun', 5),   
(106, 'I', 'Pronoun', 5),   
(107, 'in', 'Preposition', 5),   
(108, 'tooth', 'Noun', 5),
(109, 'any', 'Conjunction', 5),
(110, 'say', 'Verb', 5); 

INSERT INTO translation_tbl(id, from_word_id, to_lang_id, to_word_id, comment, alt_translation) VALUES
(1, 1, 5, 101, 'one translation', null),
(2, 2, 5, 102, 'multiple transtations and multiple sources', null),
(3, 2, 5, 105, 'multiple transtations and multiple sources', null),
(4, 3, 5, 103, 'multiple transtations', null),
(5, 3, 5, 104, 'multiple transtations', null),
(6, 4, 5, 103, 'multiple transtations', null),
(7, 4, 5, 104, 'multiple transtations', null),
(8, 5, 5, 102, 'multiple transtations and multiple sources', null),
(9, 5, 5, 105, 'multiple transtations and multiple sources', null),
(10, 6, 5, 106, 'эго?', null),
(11, 7, 5, 107, null, null),
(12, 8, 5, 108, 'one more source exists', null),
(13, 9, 5, 109, null, null),
(14, 10, 5, 110, null, null);