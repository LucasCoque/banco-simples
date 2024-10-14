Algoritmo "Rascunho v5"

Var
   //variaveis globais
   Saldo: real
   Logar: caracter
   resposta: caracter

   //função login
funcao F_Login(resposta: caracter): caractere
var
   tent: inteiro
   usuario, senha, usuario_certo, senha_certa: caractere

inicio
   tent <- 0
   usuario_certo <- "ADM"
   senha_certa <- "adm"

   escreval("============LOGIN============")
   escreval(" ")

   enquanto tent < 3 faça
      escreva("Digite o usuário: ")
      leia(usuario)
      escreva("Digite a senha: ")
      leia(senha)

      se(usuario = usuario_certo) e (senha = senha_certa) entao
         escreval("Login Bem-sucedido")
         P_Menu(Saldo)
         interrompa
      senao
         escreval("Usuário ou senha incorretos. Tente Novamente")
      fimse

      tent <- tent + 1

   fimenquanto
   retorne(resposta)
fimfuncao
//////////////////////////////////////////////////////////////////////////////

//função deposito
funcao F_Deposito(Saldo: real) : real
var
   cont, resp: inteiro
   Depost: real
inicio
   cont <- 0
   limpatela
   escreval("==========DEPÓSITO==========")
   escreval(" ")
   escreva("Digite a quantia que deseja depositar: R$")
   leia(Depost)

   Saldo <- Saldo + Depost

   escreval("Deposito realizado com sucesso!")
   escreval("=====================================================")
   escreval("Deseja realizar novo processo? Digite (1)SIM  (2)NÃO")
   escreval("=====================================================")
   leia(resp)

   enquanto cont <= 3 faca

      se (resp = 1) entao
         limpatela
         P_MENU(Saldo)
         interrompa
      fimse
      se (resp = 2) entao
         limpatela
         escreval("Programa encerrado pelo usuário")
         interrompa
      senao
         limpatela
         escreval("Operação inválida, Tente novamente")

         cont <- cont + 1
      fimse
      se cont > 2 entao
         limpatela
         escreval("Excesso de tentativas, Programa encerrado")
         interrompa
      fimse
   fimenquanto
   retorne(Saldo)
fimfuncao
//////////////////////////////////////////////////////////////////////////////

//função retirada
funcao F_Retirada(Saldo: real) : real
var
   saque: real
   cont, x, resp: inteiro

inicio
   cont <- 0

   limpatela
   escreval("==========Retirada==========")
   escreval(" ")

   enquanto x = 0 faca
      escreva("Digite o valor desejado para retirar: R$")
      leia(saque)

      se (saque <= Saldo) entao
         Saldo <- Saldo - saque

         escreval("=========================================")
         escreval("Retirada realizada com sucesso!")
         escreval("=========================================")
         interrompa
      senao
         escreval("=====================================================")
         escreval("Não foi possível realizar o saque, Saldo insuficiente")
         escreval("=====================================================")
         interrompa
         x <- 0
      fimse
   fimenquanto
   escreval("=====================================================")
   escreval("Deseja realizar novo processo? Digite (1)SIM  (2)NÃO")
   escreval("=====================================================")
   leia(resp)

   enquanto cont <= 3 faca

      se (resp = 1) entao
         limpatela
         P_MENU(Saldo)
         interrompa
      fimse
      se (resp = 2) entao
         limpatela
         escreval("Programa encerrado pelo usuário")
         interrompa
      senao
         limpatela
         escreval("Operação inválida, Tente novamente")

         cont <- cont + 1
      fimse
      se cont > 2 entao
         limpatela
         escreval("=========================================")
         escreval("Excesso de tentativas, Programa encerrado")
         escreval("=========================================")
         interrompa
      fimse
   fimenquanto
   retorne(Saldo)
fimfuncao
//////////////////////////////////////////////////////////////////////////////

//função pix
funcao F_Pix(Saldo: real) : real
var
   v_resp, v_return: inteiro
   email, cpf, cel: real
   v_transfe: real

inicio
   limpatela
   escreval("=================PIX================")
   escreval("=                                  =")
   escreval("= Selecione o formato da chave Pix =")
   escreval("=                                  =")
   escreval("=             (1)Email             =")
   escreval("=                                  =")
   escreval("=             (2)CPF               =")
   escreval("=                                  =")
   escreval("=      (3)Número de celular        =")
   escreval("=                                  =")
   escreval("====================================")
   leia(v_resp)

   escolha v_resp

   caso 1
      limpatela
      email <- F_valida_email(v_return)
      se email <> 0 entao
         v_transfe <- F_Retirada(Saldo)
      fimse
   caso 2
      limpatela
      CPF <- F_valida_cpf(v_return)
      se CPF <> 0 entao
         v_transfe <- F_Retirada(Saldo)
      fimse
   caso 3
      limpatela
      cel <- F_valida_cel(v_return)
      se cel <> 0 entao
         v_transfe <- F_Retirada(Saldo)
      fimse
   outrocaso
   fimescolha
fimfuncao

//funções de validação

//EMAIL
funcao F_valida_email(v_return: inteiro) : inteiro
var
   v_volta: real
   v_email, v_texto: caracter
   v_tent, v_aroba, v_size, v_com: inteiro
inicio
   enquanto v_tent < 3 faça

      escreval("Digite Seu email")
      leia(v_email)

      v_size <- Compr(v_email)
      v_aroba <- Pos("@"; v_email)
      v_texto <- Copia(v_email; v_aroba; v_size)
      v_com <- Pos(".com"; v_texto)

      se v_aroba > 1 entao
         se v_com > 1 entao
            v_return <- 1
            escreval("Email encontrado")
            interrompa
         fimse
      senao
         v_return <- 0
         v_tent <- v_tent + 1
         escreval("Email não encontrado")
      fimse
      se v_tent >= 3 entao
         limpatela
         escreval("=========================================")
         escreval("Excesso de tentativas, Operação cancelada")
         escreval("=========================================")
         v_volta <- f_pix(Saldo)
         interrompa
      fimse
   fimenquanto
   retorne v_return
fimfuncao

//CPF
funcao F_valida_cpf(v_return: inteiro) : inteiro
var
   v_volta: real
   v_tent,v_size: inteiro
   v_cpf: caractere
inicio

   enquanto v_tent < 3 faca
      escreva("Digite apenas os números do CPF: ")
      leia(v_cpf)
      v_size <- Compr(v_cpf)
      se v_size = 11 entao
         v_return <- 1
         limpatela
         escreval("CPF Válido")
         interrompa
      senao
         v_return <- 0
         v_tent <- v_tent + 1
         escreval("CPF Inválido. Tente novamente")
      fimse
      se v_tent >= 3 entao
         limpatela
         escreval("=========================================")
         escreval("Excesso de tentativas, Operação cancelada")
         escreval("=========================================")
         v_volta <- f_pix(Saldo)
         interrompa
      fimse
   fimenquanto
   retorne v_return
fimfuncao

//CELULAR
funcao F_valida_cel(v_return: inteiro) : inteiro
var
   v_volta: real
   v_tent, v_size: inteiro
   v_cel:caractere
inicio

   enquanto v_tent < 3 faca
      escreva("Digite apenas os números do número de celular: ")
      leia(v_cel)
      v_size <- Compr(v_cel)
      se v_size = 13 entao
         v_return <- 1
         limpatela
         escreval("Número de celular Válido")
         interrompa
      senao
         v_return <- 0
         v_tent <- v_tent + 1
         escreval("Número de celular não encontrado. Tente Novamente")
      fimse
      se v_tent >= 3 entao
         limpatela
         escreval("=========================================")
         escreval("Excesso de tentativas, Operação cancelada")
         escreval("=========================================")
         v_volta <- f_pix(Saldo)
         interrompa
      fimse
   fimenquanto
   retorne v_return
fimfuncao

//////////////////////////////////////////////////////////////////////////////

//função saldo
funcao F_Consulta(Saldo: real) : real
var
   cont, resp: inteiro

inicio
   cont <- 0
   limpatela
   escreval("============Saldo===========")
   escreval(" ")
   escreval("Seu saldo atual é de R$" , Saldo)
   escreval("=====================================================")
   escreval("Deseja realizar novo processo? Digite (1)SIM  (2)NÃO")
   escreval("=====================================================")
   leia(resp)

   enquanto cont <= 3 faca

      se (resp = 1) entao
         limpatela
         P_MENU(Saldo)
         interrompa
      fimse
      se (resp = 2) entao
         limpatela
         escreval("Programa encerrado pelo usuário")
         interrompa
      senao
         limpatela
         escreval("Operação inválida, Tente novamente")

         cont <- cont + 1
      fimse
      se cont > 2 entao
         limpatela
         escreval("Excesso de tentativas, Programa encerrado")
         interrompa
      fimse
   fimenquanto
   retorne(Saldo)
fimfuncao
//////////////////////////////////////////////////////////////////////////////

//menu
procedimento P_Menu(Saldo: real)
var
   digito: inteiro
inicio
   limpatela
   escreval("========Bem-vindo de volta!========")
   escreval("=                                 =")
   escreval("================MENU===============")
   escreval("=                                 =")
   escreval("=        (1) Despósito            =")
   escreval("=                                 =")
   escreval("=        (2) Saque                =")
   escreval("=                                 =")
   escreval("=        (3) Consultar Saldo      =")
   escreval("=                                 =")
   escreval("=        (4) Pix                  =")
   escreval("=                                 =")
   escreval("=        (5) Finalizar Programa   =")
   escreval("=                                 =")
   escreval("===================================")
   leia(digito)

   escolha digito
   caso 1
      Saldo <- F_Deposito(Saldo)
   caso 2
      Saldo <- F_Retirada(Saldo)
   caso 3
      Saldo <- F_Consulta(Saldo)
   caso 4
      Saldo <- F_Pix(Saldo)
   caso 5
      escreval("Programa encerrado")
   outrocaso
      escreval("Operação invalida")
   fimescolha

fimprocedimento
//////////////////////////////////////////////////////////////////////////////

Inicio

   //programa principal

   Logar <- F_Login(resposta)
   escreval(Logar)

   Saldo <- 0

   //////////////////////////////////////////////////////////////////////////////

Fimalgoritmo