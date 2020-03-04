ALTER TABLE word_tbl ADD COLUMN part_of_speech text NOT NULL;
ALTER TABLE word_tbl DROP CONSTRAINT word_word_lang_id_unq;
ALTER TABLE word_tbl ADD CONSTRAINT word_word_pos_lang_id_unq UNIQUE (word,part_of_speech,lang_id);