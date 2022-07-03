package bbCodeToHtml.parse;

import bbCodeToHtml.token.Tokenizer;

public interface Parser {
    String parse(Tokenizer tokenizer);
}
