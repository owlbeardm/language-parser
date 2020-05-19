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
(1, 1, 5, 101, null, null),
(2, 2, 5, 102, null, null),
(3, 2, 5, 105, null, null),
(4, 3, 5, 103, null, null),
(5, 3, 5, 104, null, null),
(6, 4, 5, 103, null, null),
(7, 4, 5, 104, null, null),
(8, 5, 5, 102, null, null),
(9, 5, 5, 105, null, null),
(10, 6, 5, 106, null, null),
(11, 7, 5, 107, null, null),
(12, 8, 5, 108, null, null),
(13, 9, 5, 109, null, null),
(14, 10, 5, 110, null, null);

INSERT INTO translation_tbl(id, from_word_id, to_lang_id, to_word_id, comment, alt_translation) VALUES
(101, 13300, 5, 101, null, null),
(102, 13330, 5, 102, null, null),
(103, 13330, 5, 105, null, null),
(104, 13360, 5, 103, null, null),
(105, 13360, 5, 104, null, null),
(106, 13390, 5, 103, null, null),
(107, 13390, 5, 104, null, null),
(108, 13420, 5, 102, null, null),
(109, 13420, 5, 105, null, null),
(110, 13450, 5, 106, null, null),
(111, 13480, 5, 107, null, null),
(112, 13510, 5, 108, null, null),
(113, 13540, 5, 109, null, null),
(114, 13570, 5, 110, null, null);

INSERT INTO translation_tbl(id, from_word_id, to_lang_id, to_word_id, comment, alt_translation) VALUES
(201, 13600, 5, 101, null, null),
(202, 13630, 5, 102, null, null),
(203, 13630, 5, 105, null, null),
(204, 13660, 5, 103, null, null),
(205, 13660, 5, 104, null, null),
(206, 13690, 5, 103, null, null),
(207, 13690, 5, 104, null, null),
(208, 13720, 5, 102, null, null),
(209, 13720, 5, 105, null, null),
(210, 13750, 5, 106, null, null),
(211, 13780, 5, 107, null, null),
(212, 13810, 5, 108, null, null),
(213, 13840, 5, 109, null, null),
(214, 13870, 5, 110, null, null);

INSERT INTO translation_tbl(id, from_word_id, to_lang_id, to_word_id, comment, alt_translation) VALUES
(301, 13900, 5, 101, null, null),
(302, 13930, 5, 102, null, null),
(303, 13930, 5, 105, null, null),
(304, 13960, 5, 103, null, null),
(305, 13960, 5, 104, null, null),
(306, 13990, 5, 103, null, null),
(307, 13990, 5, 104, null, null),
(308, 14020, 5, 102, null, null),
(309, 14020, 5, 105, null, null),
(310, 14050, 5, 106, null, null),
(311, 14080, 5, 107, null, null),
(312, 14110, 5, 108, null, null),
(313, 14140, 5, 109, null, null),
(314, 14170, 5, 110, null, null);