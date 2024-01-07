@system_r
Feature: Add new SystemRDialect implementations

    # Scenario: SystemRMultiDialect[DialectOne, DialectTwo]
    # - SystemRMultiDialect itself will impl SystemRDialect, with left-first precedence ordered
    #   merging of two dialects
    # - rework SystemRExtension trait contract to have pairs of want_foo() and do_foo() pairs;
    #   proceed in order and "choose" first dialect that "wants" any given "foo()" extension
    #   operation; return Err() if neither dialect wants
     
    # Scenario: Exclude/enforce: allow specifying constraints where certain 
    # TokenKinds/Kinds/Types are kept OUT of a Term-tree