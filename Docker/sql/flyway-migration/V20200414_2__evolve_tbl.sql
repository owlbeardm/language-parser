CREATE TABLE evolve_law_tbl (
    id bigint DEFAULT nextval('pk_sequence') NOT NULL,
    lang_from_id bigint NOT NULL,
    lang_to_id bigint NOT NULL,
    priority bigint DEFAULT 0 NOT NULL,
    sound_regex_from text NOT NULL,
    sound_to text NOT NULL,
    CONSTRAINT evolve_law_pk PRIMARY KEY (id),
    CONSTRAINT evolve_law_tbl_lang_from_id_fkey FOREIGN KEY (lang_from_id) REFERENCES language_tbl(id),
    CONSTRAINT evolve_law_tbl_lang_to_id_fkey FOREIGN KEY (lang_to_id) REFERENCES language_tbl(id),
    CONSTRAINT evolve_law_lang_from_id_lang_to_id_sound_regex_from_sound_to_unq UNIQUE (lang_from_id,lang_to_id,sound_regex_from,sound_to)
);