CREATE UNIQUE INDEX translation_from_word_id_null_alt_translation_unq ON translation_tbl (from_word_id,alt_translation) WHERE to_word_id IS NULL;
