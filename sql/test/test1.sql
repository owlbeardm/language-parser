delete from evolve_law_tbl       ;
delete from translation_tbl      ;
delete from word_origin_from_tbl ;
delete from word_origin_tbl      ;
delete from word_tbl             ;
delete from language_tbl         ;


INSERT INTO language_tbl(id, name) VALUES
(1	,	'Aboleth'),
(2	,	'Alko'),
(3	,	'ClassicalArcane'),
(4	,	'Dragon'),
(5	,	'Dwarven'),
(6	,	'Edhellen'),
(7	,	'English'),
(8	,	'Halfling'),
(9	,	'Infernal'),
(10	,	'Khuzdûl'),
(11	,	'Kobold'),
(12	,	'LizardFolk'),
(13	,	'Necril'),
(14	,	'Nerlendic'),
(15	,	'Nitholan'),
(16	,	'NitholanEmpire'),
(17	,	'OldDragon'),
(18	,	'OldNerlendic'),
(19	,	'OldNitholan'),
(20	,	'OldRunic'),
(21	,	'Orkish'),
(22	,	'PrimalMagic'),
(23	,	'ProtoCreation'),
(24	,	'ProtoDragon'),
(25	,	'ProtoDwarven'),
(26	,	'ProtoElven'),
(27	,	'ProtoHuman'),
(28	,	'ProtoMaterial'),
(29	,	'ProtoMonster'),
(30	,	'ProtoOrk'),
(31	,	'ProtoTengu'),
(32	,	'Queran'),
(33	,	'SlaveRunic'),
(34	,	'Sylvan'),
(35	,	'Titan');

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
(101, 13160, 5, 101, null, null),
(102, 13160, 5, 102, null, null),
(103, 13160, 5, 105, null, null),
(104, 13190, 5, 103, null, null),
(105, 13190, 5, 104, null, null),
(106, 13220, 5, 103, null, null),
(107, 13220, 5, 104, null, null),
(108, 13250, 5, 102, null, null),
(109, 13250, 5, 105, null, null),
(110, 13280, 5, 106, null, null),
(111, 13310, 5, 107, null, null),
(112, 13340, 5, 108, null, null),
(113, 13370, 5, 109, null, null),
(114, 13400, 5, 110, null, null);

INSERT INTO translation_tbl(id, from_word_id, to_lang_id, to_word_id, comment, alt_translation) VALUES
(201, 13430, 5, 101, null, null),
(202, 13430, 5, 102, null, null),
(203, 13430, 5, 105, null, null),
(204, 13460, 5, 103, null, null),
(205, 13460, 5, 104, null, null),
(206, 13490, 5, 103, null, null),
(207, 13490, 5, 104, null, null),
(208, 13520, 5, 102, null, null),
(209, 13520, 5, 105, null, null),
(210, 13550, 5, 106, null, null),
(211, 13580, 5, 107, null, null),
(212, 13610, 5, 108, null, null),
(213, 13640, 5, 109, null, null),
(214, 13670, 5, 110, null, null);

INSERT INTO translation_tbl(id, from_word_id, to_lang_id, to_word_id, comment, alt_translation) VALUES
(301, 13700, 5, 101, null, null),
(302, 13700, 5, 102, null, null),
(303, 13700, 5, 105, null, null),
(304, 13730, 5, 103, null, null),
(305, 13730, 5, 104, null, null),
(306, 13760, 5, 103, null, null),
(307, 13760, 5, 104, null, null),
(308, 13790, 5, 102, null, null),
(309, 13790, 5, 105, null, null),
(310, 13820, 5, 106, null, null),
(311, 13850, 5, 107, null, null),
(312, 13880, 5, 108, null, null),
(313, 13910, 5, 109, null, null),
(314, 13940, 5, 110, null, null);