# Calculadora Distribuida

Calculadora distribuida feita em erlang

# Equipe

    Evandro Padilha Barroso
    Jansen Saunier de Alcantara Junior
    Willians Amaral De Oliveira
    Yasser Schuck Antonio Tuma

# dist:

    erl -name dist@192.168.0.0 -setcookie frasesecreta
    // adicionar os nos
    net_kernel:connect_node('sum@ip').
    net_kernel:connect_node('sub@ip').
    net_kernel:connect_node('mult@ip').
    net_kernel:connect_node('div@ip').

    c(parse).
    c(dist).
    dist:start("1 + 1").

# demais:

    // servidor de soma
    erl -name sum@192.168.0.0 -setcookie frasesecreta
    c(sum_server).
    sum_server:start_link().

    //servidor de subtracao
    erl -name sub@192.168.0.0 -setcookie frasesecreta
    c(sub_server).
    sub_server:start_link().

    //servidor de mult
    erl -name mult@192.168.0.0 -setcookie frasesecreta
    c(mult_server).
    mult_server:start_link().

    //servidor de div
    erl -name div@192.168.0.0 -setcookie frasesecreta
    c(div_server).
    div_server:start_link().
