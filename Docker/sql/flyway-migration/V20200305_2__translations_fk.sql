ALTER TABLE translations_tbl  ADD CONSTRAINT from_word_id_fk FOREIGN KEY (from_word_id) REFERENCES word_tbl(id);
ALTER TABLE translations_tbl  ADD CONSTRAINT to_word_id_fk FOREIGN KEY (to_word_id) REFERENCES word_tbl(id);
ALTER TABLE translations_tbl  ADD CONSTRAINT to_lang_id_fk FOREIGN KEY (to_lang_id) REFERENCES language_tbl(id);