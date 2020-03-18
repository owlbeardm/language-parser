CREATE UNIQUE INDEX translation_from_word_id_to_word_id_null_unq ON translation_tbl (from_word_id,to_word_id) WHERE alt_translation IS NULL;
