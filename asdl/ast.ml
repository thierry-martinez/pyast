type modifier =
  | Question
  | Star

type field = {
    type_id : string;
    modifier : modifier option;
    id : string;
  }

type constructor = {
    constructor_id : string;
    fields : field list;
  }

type product = field list

type sum = constructor list

type desc =
  | Product of product
  | Sum of sum

type attributes = field list

type definition = {
    type_id : string;
    desc : desc;
    attributes : attributes;
  }

type module_ = {
    id : string;
    version : string option;
    definitions : definition list;
  }

