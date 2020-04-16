CREATE TABLE word_tbl (
    id bigint DEFAULT nextval('pk_sequence') NOT NULL,
    word text NOT NULL,
    lang_id bigint NOT NULL,
    part_of_speech text NOT NULL,
    forgotten BOOLEAN NOT NULL DEFAULT FALSE,
    CONSTRAINT word_pk PRIMARY KEY (id),
    CONSTRAINT word_tbl_lang_id_fkey FOREIGN KEY (lang_id) REFERENCES language_tbl(id)
);

CREATE INDEX wiki_word_word_idx ON word_tbl(word);

-- WordWordPosLangIdUnq word partOfSpeech langId