@system_r
Feature: Technical Debt
    #
    # Incomplete list:
    # - large amount of clones, not accounting for memory use well; Move to Rc's and a pool of unique types?
    # - Parser is broken into functions and ParserState; can it be reunified into a struct + impl? tried once and failed
    # - in line with above, rationalize state & functionality across system_r
    #    - SystemRExtensions are reified currently as a marker type/empty struct implementing a trait + dialect-state;
    #    - so along with Lexer/Context, which are structs + impl methods & parser; we have 3 distinct layouts for functionality + state;
    #    - fix this