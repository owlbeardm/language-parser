CREATE TABLE sound_law_tbl (
    id bigint DEFAULT nextval('pk_sequence')	 NOT NULL,
    rule_from text NOT NULL,
    rule_to text NOT NULL,
    CONSTRAINT sound_law_pk PRIMARY KEY (id),
    CONSTRAINT sound_law_name_unq UNIQUE (rule_from, rule_to)
);