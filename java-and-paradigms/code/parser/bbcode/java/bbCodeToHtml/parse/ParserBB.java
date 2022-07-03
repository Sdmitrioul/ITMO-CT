package bbCodeToHtml.parse;

import bbCodeToHtml.node.*;
import bbCodeToHtml.token.Token;
import bbCodeToHtml.token.TokenType;
import bbCodeToHtml.token.Tokenizer;

import java.util.ArrayList;
import java.util.List;

public class ParserBB implements Parser {
    private Tokenizer tokens;

    @Override
    public String parse(Tokenizer tokenizer) {
        tokens = tokenizer;
        return read();
    }

    private String read() {
        StringBuilder sb = new StringBuilder("");
        //List<String> list = new ArrayList<>();
        while (tokens.hasNext()) {
            Token token = tokens.next();
            switch (token.getType()) {
                case TEXT :
                    sb.append(token.getValue());
                    break;
                case NEW_LINE:
                    sb.append('\n');
                    break;
                case STRONG:
                    sb.append(new Strong(read()));
                    break;
                case EMPHASIS:
                    sb.append(new Emphasis(read()));
                    break;
                case UNDERLINED:
                    sb.append(new Underlined(read()));
                    break;
                case STRIKEOUT:
                    sb.append(new Strikeout(read()));
                    break;
                case STRONG_CLOSE:
                case EMPHASIS_CLOSE:
                case STRIKEOUT_CLOSE:
                case UNDERLINED_CLOSE:
                    return sb.toString();
                case LIST:
                    List<String> list = new ArrayList<>();
                    while (tokens.next().getType() != TokenType.LIST_CLOSE) {
                        if (tokens.current().getType() == TokenType.TEG) {
                            list.add(tokens.next().getValue());
                        }
                    }
                    sb.append(new ListBB(list));
                    break;
                case END:
                    return sb.toString();
            }
        }
        return sb.toString();
    }

}
