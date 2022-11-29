Imports System.IO
Imports System.Drawing


Public Class Form1

    Dim contador_token As Integer = 0
    Dim contador_error_1 As Integer = 0
    Dim contador_error_2 As Integer = 0
    Dim contadorLlaves As Integer = 0
    Dim contadorLlaves2 As Integer = 0
    Dim caracterlinea As Integer
 
    Public Sub metodo_analizar()
        Dim caracterAUX_1 As String = ""
        Dim caracterAUX_2 As String = ""
        Dim caracter_1 As String = ""
        Dim contadorcomillas As Integer = 0
        Dim saberSiesComentario As Boolean = False
        Dim columna_1 As Integer = 1
        Dim fila_1 As Integer = 1
        Dim examinar_ascii_1 As Integer = 0
        Dim estado As Integer = 0
        Dim examinar_1 As String = ""
        Dim texto_1 As String = areaAnalizar.Text.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter

        For puntero_1 As Integer = 0 To areaAnalizar.TextLength - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
            examinar_1 = texto_1(puntero_1)
            examinar_ascii_1 = Asc(examinar_1)
            'Console.WriteLine(examinar_ascii_1)

            Select Case estado

                Case 0
                    If examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 0
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 0
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 0
                        'para saber cuando es comentario
                    ElseIf ((examinar_ascii_1 = 33) Or (examinar_ascii_1 > 34 And examinar_ascii_1 <= 254)) And (saberSiesComentario = True) Then ' sin restriccion
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 9
                    ElseIf ((examinar_ascii_1 > 64 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 96 And examinar_ascii_1 < 123)) And (saberSiesComentario = False) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    ElseIf ((examinar_ascii_1 > 47 And examinar_ascii_1 < 58)) And (saberSiesComentario = False) Then 'numeros'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    ElseIf (examinar_ascii_1 = 209 Or examinar_ascii_1 = 241) And (saberSiesComentario = False) Then ' ñ Ñ
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    ElseIf examinar_ascii_1 = 91 Or examinar_ascii_1 = 93 Then ' [  ]
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 6
                    ElseIf examinar_ascii_1 = 123 Or examinar_ascii_1 = 125 Then ' { }
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 2
                    ElseIf examinar_ascii_1 = 59 Then ' ;
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 3
                    ElseIf examinar_ascii_1 = 61 Then ' =
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 4
                    ElseIf examinar_ascii_1 = 58 Then ' :
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 5
                    ElseIf examinar_ascii_1 = 40 Or examinar_ascii_1 = 41 Then ' ( )
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 7
                    ElseIf examinar_ascii_1 = 43 Then ' +
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 7
                    ElseIf examinar_ascii_1 = 45 Then ' -
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 7
                    ElseIf examinar_ascii_1 = 35 Then ' #
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 7
                    ElseIf examinar_ascii_1 = 44 Then ' ,
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 10
                    ElseIf examinar_ascii_1 = 34 Then ' " 
                        contadorcomillas = contadorcomillas + 1
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 8
                    ElseIf examinar_ascii_1 = 95 Then ' _
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1

                    Else
                        columna_1 = columna_1 + 1
                        Metodo_Errores(examinar_1, fila_1, columna_1)

                    End If



                Case 1
                    If (examinar_ascii_1 > 64 And examinar_ascii_1 < 91) Or (examinar_ascii_1 > 96 And examinar_ascii_1 < 123) Then 'letras'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    ElseIf (examinar_ascii_1 > 47 And examinar_ascii_1 < 58) Then  'numeros'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    ElseIf (examinar_ascii_1 = 209 Or examinar_ascii_1 = 241) Then ' ñ Ñ
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1

                        'ElseIf examinar_ascii_1 = 10 Then  'salto de linea'
                        '    fila_1 = fila_1 + 1
                        '    columna_1 = 1
                        '    estado = 1
                    ElseIf examinar_ascii_1 = 95 Then ' _
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        'caracter_1 = caracter_1 & examinar_1
                        estado = 1
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 1
                    Else
                        Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 0

                    End If

                Case 2 'para signos { } como es signo lo agrega a la tabla 
                    Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    estado = 0

                Case 3
                    Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    estado = 0

                Case 4
                    Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    estado = 0

                Case 5
                    Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    estado = 0

                Case 6
                    Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    estado = 0
                Case 7
                    If examinar_ascii_1 = 40 Or examinar_ascii_1 = 41 Or examinar_ascii_1 = 43 Or examinar_ascii_1 = 45 Or examinar_ascii_1 = 35 Then
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 7
                        'ElseIf examinar_ascii_1 = 10 Then  'salto de linea'
                        '    fila_1 = fila_1 + 1
                        '    columna_1 = 1
                        '    estado = 7
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        estado = 7
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 7
                    Else
                        Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 0
                    End If
                Case 8 'este case es para el comentario que no tiene restricciones
                    If contadorcomillas = 1 Then
                        'MessageBox.Show("SE ACTIVO COMENTARIO")
                        saberSiesComentario = True
                        Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 0

                    ElseIf contadorcomillas = 2 Then
                        'MessageBox.Show("SE CERRO COMENTARIO")
                        saberSiesComentario = False
                        contadorcomillas = 0
                        Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 0
                    End If
                Case 9
                    If ((examinar_ascii_1 = 33) Or (examinar_ascii_1 > 34 And examinar_ascii_1 <= 254)) Then ' sin restriccion
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 9
                    ElseIf (examinar_ascii_1 = 34) Then  ' " 
                        Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 0
                    ElseIf examinar_ascii_1 = 10 Then  'salto de linea'
                        fila_1 = fila_1 + 1
                        columna_1 = 1
                        estado = 9
                    ElseIf examinar_ascii_1 = 32 Then   'espacio en blanco'
                        columna_1 = columna_1 + 1
                        caracter_1 = caracter_1 & examinar_1
                        estado = 9
                    ElseIf examinar_ascii_1 = 9 Then 'tabulacion'
                        columna_1 = columna_1 + 1
                        estado = 9
                    Else
                        Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                        caracter_1 = ""
                        puntero_1 = puntero_1 - 1
                        estado = 0

                    End If

                Case 10
                    Metodo2_tokens(caracter_1, fila_1, columna_1, caracterAUX_1, caracterAUX_2) 'llenar campos de envio lexico'
                    caracter_1 = ""
                    puntero_1 = puntero_1 - 1
                    estado = 0
            End Select
        Next

        

    End Sub

    Public Sub Metodo_Errores(ByVal examinar_1 As String, ByVal fila_1 As Integer, ByVal columna_1 As Integer)

        contador_error_1 = contador_error_1 + 1
        dgverror.Rows.Add(contador_error_1, examinar_1, columna_1, fila_1, "Simbolo No reconocido")

    End Sub

    Public Sub Metodo2_tokens(ByVal caracter_1 As String, ByVal fila_1 As Integer, ByVal columna_1 As Integer, ByVal caracterAUX_1 As String, ByVal caracterAUX_2 As String)


        Dim no_token As Integer = 0
        Dim eltoken As String = ""   'tipo token'
        Dim valornumerico As Integer = 0

        Dim token_1 As String = "{"
        Dim token_2 As String = "}"
        Dim token_3 As String = "clase"
        Dim token_4 As String = "nombre"
        Dim token_5 As String = ";"
        Dim token_6 As String = "="
        Dim token_7 As String = "atributos"
        Dim token_8 As String = ":"
        Dim token_9 As String = "(+)"
        Dim token_10 As String = "(-)"
        Dim token_11 As String = "(#)"
        Dim token_12 As String = "metodos"
        Dim token_13 As String = "asociacion"
        Dim token_14 As String = "agregacion"
        Dim token_15 As String = "composicion"
        Dim token_16 As String = "herencia"
        Dim token_17 As String = "asociacionsimple"
        Dim token_18 As String = "["
        Dim token_19 As String = "]"
        Dim token_20 As String = ")"
        Dim token_21 As String = "("
        Dim token_22 As String = "+"
        Dim token_23 As String = "-"
        Dim token_24 As String = "#"
        Dim token_25 As String = "comentario"
        Dim token_26 As String = "texto"
        Dim token_27 As String = "color"
        Dim token_28 As String = ","
        Dim token_29 As String = Chr(34)
        Dim token_30 As String = ""

        ' Dim token_13 As String = "ID"

        Dim var_caracter As String = ""
        Dim var_tipotoke As String = ""
        Dim var_notoken As Integer = 0
        Dim var_posicion As String = ""

        If (Integer.TryParse(caracter_1, valornumerico)) Then
            token_30 = caracter_1
        End If

        Select Case LCase(caracter_1)
            Case token_1
                eltoken = "{"
                no_token = 1
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo {"
                var_notoken = 1
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_2
                eltoken = "}"
                no_token = 2
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo }"
                var_notoken = 2
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_3
                eltoken = caracter_1
                no_token = 3
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 3
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_4
                eltoken = "Nombre"
                no_token = 4
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 4
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_5
                eltoken = ";"
                no_token = 5
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo ;"
                var_notoken = 5
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_6
                eltoken = "="
                no_token = 6
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo ="
                var_notoken = 6
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_7
                eltoken = "Atributos"
                no_token = 7
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 7
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_8
                eltoken = ":"
                no_token = 8
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo :"
                var_notoken = 8
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_9
                eltoken = "(+)"
                no_token = 9
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Visibilidad"
                var_notoken = 9
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_10
                eltoken = "(-)"
                no_token = 10
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Visibilidad"
                var_notoken = 10
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_11
                eltoken = "(#)"
                no_token = 11
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Visibilidad"
                var_notoken = 11
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_12
                eltoken = "Metodos"
                no_token = 12
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 12
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_13
                eltoken = "Asociacion"
                no_token = 13
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 13
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_14
                eltoken = "Agregacion"
                no_token = 14
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 14
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_15
                eltoken = "Composicion"
                no_token = 15
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 15
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_16
                eltoken = "Herencia"
                no_token = 16
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 16
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_17
                eltoken = "AsociacionSimple"
                no_token = 17
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 17
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_18
                eltoken = "["
                no_token = 18
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo ["
                var_notoken = 18
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_19
                eltoken = "]"
                no_token = 17
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo ]"
                var_notoken = 17
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_20
                eltoken = ")"
                no_token = 20
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo )"
                var_notoken = 15
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_21
                eltoken = "("
                no_token = 16
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo ("
                var_notoken = 16
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_22
                eltoken = "AsociacionSimple"
                no_token = 17
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Visibilidad"
                var_notoken = 17
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_23
                eltoken = "-"
                no_token = 18
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Visibilidad"
                var_notoken = 18
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_24
                eltoken = "#"
                no_token = 17
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Visibilidad"
                var_notoken = 17
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_25
                eltoken = "texto"
                no_token = 16
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 16
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_26
                eltoken = "comentario"
                no_token = 17
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 17
                var_posicion = "fila" & fila_1 & "columna" & columna_1
            Case token_27
                eltoken = "color"
                no_token = 17
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Palabra Reservada"
                var_notoken = 17
                var_posicion = "fila" & fila_1 & "columna" & columna_1
            Case token_28
                eltoken = ","
                no_token = 1
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo ,"
                var_notoken = 1
                var_posicion = "fila" & fila_1 & "columna" & columna_1

            Case token_29
                eltoken = Chr(34)
                no_token = 1
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Signo Comilla"
                var_notoken = 1
                var_posicion = "fila" & fila_1 & "columna" & columna_1
            Case token_30
                contador_token = contador_token + 1

                var_caracter = caracter_1
                var_tipotoke = "Numerico"
                var_notoken = 1
                var_posicion = "fila" & fila_1 & "columna" & columna_1
            Case Else
                contador_token = contador_token + 1
                var_caracter = caracter_1
                var_tipotoke = "Identificador"
                var_notoken = 6
                var_posicion = "fila" & fila_1 & "columna" & columna_1

        End Select

       
        dgvbuena.Rows.Add(contador_token, caracter_1, var_tipotoke, columna_1, fila_1)

    End Sub

    Public Sub pintarLetras()

        'PARA PINTAR LAS LETRAS
        Dim q As Integer
        Dim clase As String = "clase"
        Dim nombre As String = "nombre"
        Dim atributos As String = "atributos"
        Dim metodos As String = "metodos"
        Dim asociacion As String = "asociacion"
        Dim agregacion As String = "agregacion"
        Dim composicion As String = "composicion"
        Dim herencia As String = "herencia"
        Dim asociacionsimple As String = "asociacionsimple"
        Dim texto As String = "texto"
        Dim comentario As String = "comentario"
        Dim color1 As String = "color"

        For q = 0 To areaAnalizar.TextLength - 5
            If LCase(areaAnalizar.Text.Substring(q, 5)) = clase Then
                areaAnalizar.Select(q, 5)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 6
            If LCase(areaAnalizar.Text.Substring(q, 6)) = nombre Then
                areaAnalizar.Select(q, 6)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 9
            If LCase(areaAnalizar.Text.Substring(q, 9)) = atributos Then
                areaAnalizar.Select(q, 9)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 7
            If LCase(areaAnalizar.Text.Substring(q, 7)) = metodos Then
                areaAnalizar.Select(q, 7)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 10
            If LCase(areaAnalizar.Text.Substring(q, 10)) = asociacion Then
                areaAnalizar.Select(q, 10)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 10
            If LCase(areaAnalizar.Text.Substring(q, 10)) = agregacion Then
                areaAnalizar.Select(q, 10)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 11
            If LCase(areaAnalizar.Text.Substring(q, 11)) = composicion Then
                areaAnalizar.Select(q, 11)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 8
            If LCase(areaAnalizar.Text.Substring(q, 8)) = herencia Then
                areaAnalizar.Select(q, 8)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 16
            If LCase(areaAnalizar.Text.Substring(q, 16)) = asociacionsimple Then
                areaAnalizar.Select(q, 16)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 1
            If LCase(areaAnalizar.Text.Substring(q, 1)) = "[" Or LCase(areaAnalizar.Text.Substring(q, 1)) = "]" Or LCase(areaAnalizar.Text.Substring(q, 1)) = "{" Or LCase(areaAnalizar.Text.Substring(q, 1)) = "}" Or LCase(areaAnalizar.Text.Substring(q, 1)) = "(" Or LCase(areaAnalizar.Text.Substring(q, 1)) = ")" Then
                areaAnalizar.Select(q, 1)
                areaAnalizar.SelectionColor = Color.Red
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 1
            If LCase(areaAnalizar.Text.Substring(q, 1)) = "." Or LCase(areaAnalizar.Text.Substring(q, 1)) = ":" Or LCase(areaAnalizar.Text.Substring(q, 1)) = ";" Or LCase(areaAnalizar.Text.Substring(q, 1)) = "," Or LCase(areaAnalizar.Text.Substring(q, 1)) = "=" Then
                areaAnalizar.Select(q, 1)
                areaAnalizar.SelectionColor = Color.Black
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 1
            If LCase(areaAnalizar.Text.Substring(q, 1)) = "+" Or LCase(areaAnalizar.Text.Substring(q, 1)) = "-" Or LCase(areaAnalizar.Text.Substring(q, 1)) = "#" Then
                areaAnalizar.Select(q, 1)
                areaAnalizar.SelectionColor = Color.Green
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 5
            If LCase(areaAnalizar.Text.Substring(q, 5)) = texto Then
                areaAnalizar.Select(q, 5)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 10
            If LCase(areaAnalizar.Text.Substring(q, 10)) = comentario Then
                areaAnalizar.Select(q, 10)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 1
            If LCase(areaAnalizar.Text.Substring(q, 1)) = Chr(34) Then
                areaAnalizar.Select(q, 1)
                areaAnalizar.SelectionColor = Color.Purple
            End If
        Next
        For q = 0 To areaAnalizar.TextLength - 5
            If LCase(areaAnalizar.Text.Substring(q, 5)) = color1 Then
                areaAnalizar.Select(q, 5)
                areaAnalizar.SelectionColor = Color.Yellow
            End If
        Next


        'PARA PINTAR VISIVILIDAD CUANDO VIENE CON ESPACIOS: (       +      )
        'Dim mychar As String
        'Dim abre As Boolean = False
        'Dim cierra As Boolean = False
        'Dim abrenum As Integer = 0
        'Dim cierranum As Integer = 0
        'Dim cadena As String = areaAnalizar.Text
        'Dim cadenapintar As String = ""
        'Dim tamaniocadenapintar As Integer = 0

        'For i = 0 To areaAnalizar.Text.Length - 1
        '    mychar = areaAnalizar.Text.Chars(i)

        '    If mychar = "(" Then
        '        abre = True
        '        abrenum = i
        '    ElseIf mychar = ")" Then
        '        cierra = True
        '        cierranum = i
        '    End If
        '    If abre = True And cierra = True Then
        '        'Console.WriteLine(abrenum)
        '        'Console.WriteLine(cierranum)

        '        Try
        '            If cadena.Substring(abrenum, cierranum - abrenum).Contains("+") Or cadena.Substring(abrenum, cierranum - abrenum).Contains("-") Or cadena.Substring(abrenum, cierranum - abrenum).Contains("#") Then
        '                cadenapintar = (cadena.Substring(abrenum, cierranum - abrenum + 1))
        '                tamaniocadenapintar = (cierranum - abrenum + 1)

        '                For q = 0 To areaAnalizar.TextLength - tamaniocadenapintar
        '                    If LCase(areaAnalizar.Text.Substring(q, tamaniocadenapintar)) = cadenapintar Then
        '                        areaAnalizar.Select(q, tamaniocadenapintar)
        '                        areaAnalizar.SelectionColor = Color.Green
        '                    End If
        '                Next

        '            Else
        '                'Console.WriteLine(cadena.Substring(abrenum, cierranum - abrenum + 1))
        '            End If
        '        Catch ex As ArgumentOutOfRangeException

        '        End Try

        '        abre = False
        '        cierra = False
        '    End If
        'Next

        ''PARA ERRORES
        'For Each row As DataGridViewRow In dgverror.Rows

        '    If row.Cells(1).Value <> "" Then
        '        For q = 0 To areaAnalizar.TextLength - 1
        '            If LCase(areaAnalizar.Text.Substring(q, 1)) = row.Cells(1).Value Then
        '                areaAnalizar.Select(q, 1)
        '                areaAnalizar.SelectionColor = Color.Blue
        '            End If
        '        Next
        '    End If
        'Next

        'PARA IDENTIFICADORES Y NUMEROS
        Dim contadorrr2 As Integer = 0
        Dim cadenaa2 As String = ""
        For Each row3 As DataGridViewRow In dgvbuena.Rows

            Try
                cadenaa2 = row3.Cells(1).Value
                If (dgvbuena.Rows(contadorrr2).Cells(2).Value = "Identificador" Or dgvbuena.Rows(contadorrr2).Cells(2).Value = "Numerico") And dgvbuena.Rows(contadorrr2 - 1).Cells(1).Value <> Chr(34) And dgvbuena.Rows(contadorrr2 + 1).Cells(1).Value <> Chr(34) Then
                    cadenaa2 = LCase(dgvbuena.Rows(contadorrr2).Cells(1).Value)
                    For q = 0 To areaAnalizar.TextLength - (cadenaa2.Length)
                        If LCase(areaAnalizar.Text.Substring(q, cadenaa2.Length)) = cadenaa2 Then
                            areaAnalizar.Select(q, cadenaa2.Length)
                            areaAnalizar.SelectionColor = Color.Orange
                        End If
                    Next
                End If
            Catch ex As Exception

            End Try

            contadorrr2 = contadorrr2 + 1
        Next

        'PARA COMENTARIO
        Dim contadorrr As Integer = 0
        Dim cadenaa As String = ""
        For Each row2 As DataGridViewRow In dgvbuena.Rows

            Try
                cadenaa = row2.Cells(1).Value
                If dgvbuena.Rows(contadorrr).Cells(2).Value = "Identificador" And dgvbuena.Rows(contadorrr - 1).Cells(1).Value = Chr(34) And dgvbuena.Rows(contadorrr + 1).Cells(1).Value = Chr(34) Then
                    cadenaa = LCase(dgvbuena.Rows(contadorrr).Cells(1).Value)
                    For q = 0 To areaAnalizar.TextLength - (cadenaa.Length)
                        If LCase(areaAnalizar.Text.Substring(q, cadenaa.Length)) = cadenaa Then
                            areaAnalizar.Select(q, cadenaa.Length)
                            areaAnalizar.SelectionColor = Color.Purple
                        End If
                    Next
                End If
            Catch ex As Exception

            End Try

            contadorrr = contadorrr + 1
        Next

        


    End Sub

    Public Sub llenarCola()

        Dim contador As Integer = 0 'contador para ver en que fila me encuentro
        Dim bloquenombre As Boolean = False
        Dim bloqueclase As Boolean = False 'para saber donde abre y cierra bloque clase
        Dim bloqueatributos As Boolean = False 'para saber donde abre y cierra bloque atributos
        Dim bloquemetodos As Boolean = False 'para saber donde abre y cierra bloque metodos
        Dim bloqueasociacion As Boolean = False
        Dim bloquehayColor As Boolean = False
        Dim bloqueComentario As Boolean = False
        Dim nombrecomentarioo As String = ""
        Dim textocomentario As String = ""
        Dim colaClase As New Queue(Of String)
        Dim colaAtributos As New Queue(Of String)
        Dim colaMetodos As New Queue(Of String)
        Dim colaColores As New Queue(Of String)
        Dim colorHexa As String = "#"
        Dim contadorCola As Integer = 0
        Dim valorNumerico As Integer = 0 'para saber si los colores contienen letras
        Dim nombredemiclase As String = ""
        Dim aqueclasevacomentario As String = "" 'ya no se utiliza
        Dim quecomentariotieneclase As String = ""

        'variables para verificar que datos tengo
        Dim clase As String = "clase"
        Dim nombre As String = "nombre"
        Dim atributos As String = "atributos"
        Dim metodos As String = "metodos"
        Dim asociacion As String = "asociacion"
        Dim agregacion As String = "agregacion"
        Dim composicion As String = "composicion"
        Dim herencia As String = "herencia"
        Dim asociacionsimple As String = "asociacionsimple"
        Dim color As String = "color"
        Dim comentario As String = "comentario"
        Dim texto As String = "texto"

        For Each row As DataGridViewRow In dgvbuena.Rows 'para recorrer todas las filas de mi tabla

            Try
                'para saber si la CLASE SE ABRIO
                If bloqueclase = False And (row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = clase) And dgvbuena.Rows(contador - 1).Cells(1).Value = "[" And dgvbuena.Rows(contador + 1).Cells(1).Value = "]" And dgvbuena.Rows(contador + 2).Cells(1).Value = "{" Then
                    Console.WriteLine("--------------------SE ABRIO CLASE--------------------------")
                    bloqueclase = True
                End If

                'Si tiene nombre
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = nombre And dgvbuena.Rows(contador - 1).Cells(1).Value = "[" And dgvbuena.Rows(contador + 1).Cells(1).Value = "]" And dgvbuena.Rows(contador + 2).Cells(1).Value = "=" And bloqueclase = True Then
                    bloquenombre = True
                    Console.WriteLine("TIENE NOMBRE")

                End If

                'nombre de mi clase
                If row.Cells(2).Value = "Identificador" And dgvbuena.Rows(contador - 1).Cells(1).Value = "=" And dgvbuena.Rows(contador + 1).Cells(1).Value = ";" And bloqueclase = True And bloquenombre = True Then
                    Console.WriteLine("Nombre de mi clase: " & dgvbuena.Rows(contador).Cells(1).Value) ' entonces me imprime el nombre de mi clase
                    nombredemiclase = dgvbuena.Rows(contador).Cells(1).Value

                    'para validar el nombre de mi clase: Letra+(“_”)?(Letra|Digito)*
                    If (nombredemiclase.Substring(0, 1) = "_") Then
                        contador_error_1 = contador_error_1 + 1
                        dgverror.Rows.Add(contador_error_1, nombredemiclase, dgvbuena.Rows(contador).Cells(3).Value, dgvbuena.Rows(contador).Cells(4).Value, "No cumple con la expresion")
                    ElseIf nombredemiclase.IndexOf("_", 0) > 1 Then
                        contador_error_1 = contador_error_1 + 1
                        dgverror.Rows.Add(contador_error_1, nombredemiclase, dgvbuena.Rows(contador).Cells(3).Value, dgvbuena.Rows(contador).Cells(4).Value, "No cumple con la expresion")
                    ElseIf nombredemiclase.IndexOf("_", 0) = 1 And nombredemiclase.Length = 2 Then
                        contador_error_1 = contador_error_1 + 1
                        dgverror.Rows.Add(contador_error_1, nombredemiclase, "No cumple con la expresion", dgvbuena.Rows(contador).Cells(3).Value, dgvbuena.Rows(contador).Cells(4).Value)
                    End If

                    'para ver si cierra mi clase
                    If dgvbuena.Rows(contador + 2).Cells(1).Value = "}" Then

                        'para ver si mi clase es interfaz o clase normal, si colaatributos=0 quiere decir que no hay atributos
                        If colaAtributos.Count = 0 Then
                            colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & "\<\<interfaz\>\> \n " & nombredemiclase & "|")
                            llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                            colaClase.Clear()
                            colaAtributos.Clear()
                            colaMetodos.Clear()
                            colaColores.Clear()
                        Else
                            colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & nombredemiclase & "|")
                            llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                            colaClase.Clear()
                            colaAtributos.Clear()
                            colaMetodos.Clear()
                            colaColores.Clear()
                        End If
                        dgvatributosMetodos.Rows.Clear()
                        Console.WriteLine("---------------SE CERRO CLASE--------------------")
                        bloqueclase = False
                    End If

                End If

                'Si tiene color
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = color And dgvbuena.Rows(contador - 1).Cells(1).Value = "[" And dgvbuena.Rows(contador + 1).Cells(1).Value = "]" And dgvbuena.Rows(contador + 2).Cells(1).Value = "=" And bloqueclase = True Then
                    bloquehayColor = True
                    Console.WriteLine("TIENE COLOR")

                End If

                'Para ver los Colores
                If bloquehayColor = True And dgvbuena.Rows(contador + 4).Cells(1).Value = "," And dgvbuena.Rows(contador + 6).Cells(1).Value = "," And dgvbuena.Rows(contador + 8).Cells(1).Value = ";" Then

                    'Para saber si uno de los tres numeros de los colores contiene letras
                    If (Integer.TryParse(dgvbuena.Rows(contador + 3).Cells(1).Value, valorNumerico)) Then

                        If (Integer.TryParse(dgvbuena.Rows(contador + 5).Cells(1).Value, valorNumerico)) Then

                            If (Integer.TryParse(dgvbuena.Rows(contador + 7).Cells(1).Value, valorNumerico)) Then

                                'imprimir que colores tiene
                                Console.WriteLine("Sus Colores son: " + dgvbuena.Rows(contador + 3).Cells(1).Value + " " + dgvbuena.Rows(contador + 5).Cells(1).Value + " " + dgvbuena.Rows(contador + 7).Cells(1).Value)
                                colaColores.Enqueue(" ,style=filled,fillcolor= " + Chr(34) + colores(Convert.ToInt32(dgvbuena.Rows(contador + 3).Cells(1).Value), Convert.ToInt32(dgvbuena.Rows(contador + 5).Cells(1).Value), Convert.ToInt32(dgvbuena.Rows(contador + 7).Cells(1).Value)) + Chr(34))
                                bloquehayColor = False ' para que cierre bleque

                                'para ver si cierra la clase
                                If dgvbuena.Rows(contador + 9).Cells(1).Value = "}" Then

                                    'para ver si mi clase es interfaz o clase normal, si colaatributos=0 quiere decir que no hay atributos
                                    If colaAtributos.Count = 0 Then
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & "\<\<interfaz\>\> \n " & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    Else
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    End If
                                    dgvatributosMetodos.Rows.Clear()
                                    Console.WriteLine("---------------SE CERRO CLASE--------------------")
                                    bloqueclase = False
                                    'para llenar cola en graphiz


                                End If


                            Else
                                contador_error_2 = contador_error_2 + 1
                                dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 7).Cells(1).Value, "Para declarar color tiene que ser un valor Numerico", dgvbuena.Rows(contador + 7).Cells(3).Value, dgvbuena.Rows(contador + 7).Cells(4).Value)
                            End If

                        Else
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 5).Cells(1).Value, "Para declarar color tiene que ser un valor Numerico", dgvbuena.Rows(contador + 7).Cells(3).Value, dgvbuena.Rows(contador + 7).Cells(4).Value)
                        End If

                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 3).Cells(1).Value, "Para declarar color tiene que ser un valor Numerico", dgvbuena.Rows(contador + 7).Cells(3).Value, dgvbuena.Rows(contador + 7).Cells(4).Value)

                    End If

                End If


                'Si tiene atributos
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = atributos And dgvbuena.Rows(contador - 1).Cells(1).Value = "[" And dgvbuena.Rows(contador + 1).Cells(1).Value = "]" And dgvbuena.Rows(contador + 2).Cells(1).Value = "{" And bloqueclase = True Then
                    bloqueatributos = True
                    Console.WriteLine("TIENE ATRIBUTOS")
                End If

                'para ver que atributos tiene
                If bloqueatributos = True And row.Cells(2).Value = "Visibilidad" And (row.Cells(1).Value = "(+)" Or row.Cells(1).Value = "(-)" Or row.Cells(1).Value = "(#)") Then
                    'para ver si mi atributo tiene alguna palabra reservada
                    If LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = clase Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = nombre Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = atributos Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = metodos Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = asociacion Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = agregacion Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = composicion Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = herencia Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = asociacion Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = texto Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = comentario Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = color Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "El nombre del atributo no puede ser una palabra reservada", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                    Else
                        'para ver si mi atributo tiene TIPO
                        If dgvbuena.Rows(contador + 2).Cells(1).Value = ";" Then
                            Console.WriteLine(dgvbuena.Rows(contador).Cells(1).Value + dgvbuena.Rows(contador + 1).Cells(1).Value + dgvbuena.Rows(contador + 2).Cells(1).Value)

                            'para llenar mi cola para graphiz
                            colaAtributos.Enqueue(" \n " & dgvbuena.Rows(contador).Cells(1).Value & dgvbuena.Rows(contador + 1).Cells(1).Value)

                            'PARA VERIFICAR QUESI SE REPITEN MIS ATRIBUTOS
                            For Each row2 As DataGridViewRow In dgvatributosMetodos.Rows
                                If row2.Cells(0).Value = dgvbuena.Rows(contador + 1).Cells(1).Value Then
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "El atributo ya existe", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                                End If
                            Next
                            dgvatributosMetodos.Rows.Add(dgvbuena.Rows(contador + 1).Cells(1).Value)

                            'para cerrar bloque atributo por si es el ultimo
                            If dgvbuena.Rows(contador + 3).Cells(1).Value = "}" Then
                                bloqueatributos = False
                                'para cerrar la clase si atributos es de ultimo
                                If dgvbuena.Rows(contador + 4).Cells(1).Value = "}" Then

                                    'para ver si mi clase es interfaz o clase normal, si colaatributos=0 quiere decir que no hay atributos
                                    If colaAtributos.Count = 0 Then
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & "\<\<interfaz\>\> \n " & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    Else
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    End If
                                    dgvatributosMetodos.Rows.Clear()
                                    Console.WriteLine("---------------SE CERRO CLASE--------------------")
                                    bloqueclase = False

                                End If

                            End If

                        Else
                            Console.WriteLine(dgvbuena.Rows(contador).Cells(1).Value + dgvbuena.Rows(contador + 1).Cells(1).Value + dgvbuena.Rows(contador + 2).Cells(1).Value + dgvbuena.Rows(contador + 3).Cells(1).Value)
                            'para llenar mi cola para graphiz
                            colaAtributos.Enqueue(" \n " & dgvbuena.Rows(contador).Cells(1).Value & dgvbuena.Rows(contador + 1).Cells(1).Value & dgvbuena.Rows(contador + 2).Cells(1).Value & dgvbuena.Rows(contador + 3).Cells(1).Value)

                            'PARA VERIFICAR QUESI SE REPITEN MIS ATRIBUTOS
                            For Each row2 As DataGridViewRow In dgvatributosMetodos.Rows
                                If row2.Cells(0).Value = dgvbuena.Rows(contador + 1).Cells(1).Value Then
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "El atributo ya existe", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                                End If
                            Next
                            dgvatributosMetodos.Rows.Add(dgvbuena.Rows(contador + 1).Cells(1).Value)

                            'para cerrar bloque atributo por si es el ultimo
                            If dgvbuena.Rows(contador + 5).Cells(1).Value = "}" Then
                                bloqueatributos = False
                                'para cerrar la clase si atributos es de ultimo
                                If dgvbuena.Rows(contador + 6).Cells(1).Value = "}" Then

                                    'para ver si mi clase es interfaz o clase normal, si colaatributos=0 quiere decir que no hay atributos
                                    If colaAtributos.Count = 0 Then
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & "\<\<interfaz\>\> \n " & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    Else
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    End If
                                    dgvatributosMetodos.Rows.Clear()
                                    Console.WriteLine("---------------SE CERRO CLASE--------------------")
                                    bloqueclase = False
                                End If
                            End If
                        End If

                    End If

                End If


                'Si tiene metodos
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = metodos And dgvbuena.Rows(contador - 1).Cells(1).Value = "[" And dgvbuena.Rows(contador + 1).Cells(1).Value = "]" And dgvbuena.Rows(contador + 2).Cells(1).Value = "{" And bloqueclase = True Then
                    bloquemetodos = True
                    Console.WriteLine("TIENE METODOS")
                    dgvatributosMetodos.Rows.Clear()
                End If

                'para ver que metodos tiene
                If bloquemetodos = True And row.Cells(2).Value = "Visibilidad" And (row.Cells(1).Value = "(+)" Or row.Cells(1).Value = "(-)" Or row.Cells(1).Value = "(#)") Then
                    'para ver si mi metodo tiene alguna palabra reservada
                    If LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = clase Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = nombre Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = atributos Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = metodos Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = asociacion Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = agregacion Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = composicion Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = herencia Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = asociacion Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = texto Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = comentario Or LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = color Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "El nombre del metodo no puede ser una palabra reservada", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                    Else
                        'para ver si mi metodo tiene TIPO
                        If dgvbuena.Rows(contador + 2).Cells(1).Value = ";" Then
                            Console.WriteLine(dgvbuena.Rows(contador).Cells(1).Value + dgvbuena.Rows(contador + 1).Cells(1).Value + dgvbuena.Rows(contador + 2).Cells(1).Value)
                            'para llenar mi cola para graphiz
                            colaMetodos.Enqueue(" \n " & dgvbuena.Rows(contador).Cells(1).Value & dgvbuena.Rows(contador + 1).Cells(1).Value & dgvbuena.Rows(contador + 2).Cells(1).Value)

                            'PARA VERIFICAR QUESI SE REPITEN MIS METODOS
                            For Each row2 As DataGridViewRow In dgvatributosMetodos.Rows
                                If row2.Cells(0).Value = dgvbuena.Rows(contador + 1).Cells(1).Value Then
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "El Metodo ya existe", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                                End If
                            Next
                            dgvatributosMetodos.Rows.Add(dgvbuena.Rows(contador + 1).Cells(1).Value)

                            'para cerrar bloque metodo por si es el ultimo
                            If dgvbuena.Rows(contador + 3).Cells(1).Value = "}" Then
                                bloquemetodos = False
                                'para cerrar la clase si metodos es de ultimo
                                If dgvbuena.Rows(contador + 4).Cells(1).Value = "}" Then

                                    'para ver si mi clase es interfaz o clase normal, si colaatributos=0 quiere decir que no hay atributos
                                    If colaAtributos.Count = 0 Then
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & "\<\<interfaz\>\> \n " & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    Else
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    End If
                                    dgvatributosMetodos.Rows.Clear()
                                    Console.WriteLine("---------------SE CERRO CLASE--------------------")
                                    bloqueclase = False

                                End If
                            End If

                        Else
                            Console.WriteLine(dgvbuena.Rows(contador).Cells(1).Value + dgvbuena.Rows(contador + 1).Cells(1).Value + dgvbuena.Rows(contador + 2).Cells(1).Value + dgvbuena.Rows(contador + 3).Cells(1).Value)
                            'para llenar mi cola para graphiz
                            colaMetodos.Enqueue(" \n " & dgvbuena.Rows(contador).Cells(1).Value & dgvbuena.Rows(contador + 1).Cells(1).Value & dgvbuena.Rows(contador + 2).Cells(1).Value & dgvbuena.Rows(contador + 3).Cells(1).Value)

                            'PARA VERIFICAR QUESI SE REPITEN MIS METODOS
                            For Each row2 As DataGridViewRow In dgvatributosMetodos.Rows
                                If row2.Cells(0).Value = dgvbuena.Rows(contador + 1).Cells(1).Value Then
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "El Metodo ya existe", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                                End If
                            Next
                            dgvatributosMetodos.Rows.Add(dgvbuena.Rows(contador + 1).Cells(1).Value)

                            'para cerrar bloque metodo por si es el ultimo
                            If dgvbuena.Rows(contador + 5).Cells(1).Value = "}" Then
                                bloquemetodos = False
                                'para cerrar la clase si metodos es de ultimo
                                If dgvbuena.Rows(contador + 6).Cells(1).Value = "}" Then

                                    'para ver si mi clase es interfaz o clase normal, si colaatributos=0 quiere decir que no hay atributos
                                    If colaAtributos.Count = 0 Then
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & "\<\<interfaz\>\> \n " & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    Else
                                        colaClase.Enqueue(nombredemiclase & " [label = " & Chr(34) & nombredemiclase & "|")
                                        llenarCola2(colaClase, colaAtributos, colaMetodos, colaColores)
                                        colaClase.Clear()
                                        colaAtributos.Clear()
                                        colaMetodos.Clear()
                                        colaColores.Clear()
                                    End If
                                    dgvatributosMetodos.Rows.Clear()
                                    Console.WriteLine("---------------SE CERRO CLASE--------------------")
                                    bloqueclase = False

                                End If
                            End If
                        End If

                    End If

                End If

                'para comentario
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = comentario And dgvbuena.Rows(contador - 1).Cells(1).Value = "[" And dgvbuena.Rows(contador + 1).Cells(1).Value = "]" And dgvbuena.Rows(contador + 2).Cells(1).Value = "{" Then
                    bloqueComentario = True
                    Console.WriteLine("-----------------BLOQUE COMENTARIO-----------------------------")

                End If

                'a que clase va el comentario
                If bloqueComentario = True And row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = nombre And dgvbuena.Rows(contador - 1).Cells(1).Value = "[" And dgvbuena.Rows(contador + 1).Cells(1).Value = "]" And dgvbuena.Rows(contador + 2).Cells(1).Value = "=" Then
                    Console.WriteLine("Nombre de Comentario " + dgvbuena.Rows(contador + 3).Cells(1).Value)
                    nombrecomentarioo = dgvbuena.Rows(contador + 3).Cells(1).Value
                    'para ver si cierra el bloque aca
                    If dgvbuena.Rows(contador + 5).Cells(1).Value = "}" Then
                        tablagrafo3.Rows.Add(nombrecomentarioo, textocomentario)
                        nombrecomentarioo = ""
                        textocomentario = ""
                        bloqueComentario = False
                        Console.WriteLine("---------------SE CERRO COMENTARIO--------------------")

                    End If

                End If

                'que texto lleva el comentario
                If bloqueComentario = True And row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = texto And dgvbuena.Rows(contador - 1).Cells(1).Value = "[" And dgvbuena.Rows(contador + 1).Cells(1).Value = "]" And dgvbuena.Rows(contador + 2).Cells(1).Value = "=" Then
                    Console.WriteLine("El texto dice: " + dgvbuena.Rows(contador + 4).Cells(1).Value)
                    textocomentario = dgvbuena.Rows(contador + 4).Cells(1).Value
                    'tablagrafo3.Rows.Add(Chr(34) & aqueclasevacomentario & Chr(34) & " -> " & Chr(34) & dgvbuena.Rows(contador + 4).Cells(1).Value & Chr(34) & " [style=" & Chr(34) & "dashed" & Chr(34) & "]")
                    'para ver si cierra el bloque aca
                    
                    If dgvbuena.Rows(contador + 7).Cells(1).Value = "}" Then
                        'nombrecomentarioo = ""
                        'textocomentario = ""
                        tablagrafo3.Rows.Add(nombrecomentarioo, textocomentario)
                        nombrecomentarioo = ""
                        textocomentario = ""
                        bloqueComentario = False
                        Console.WriteLine("---------------SE CERRO COMENTARIO--------------------")
                    End If
                End If

               


            Catch ex As ArgumentOutOfRangeException
                'MessageBox.Show("Fuera de intervalo")
            End Try




            contador += 1 'contador aumenta en 1
        Next


    End Sub

    'me agrega la clase con metodos y atributos a una tabla
    Public Sub llenarCola2(ByVal clase As Queue(Of String), ByVal atributos As Queue(Of String), ByVal metodos As Queue(Of String), ByVal color As Queue(Of String))

        Dim codigoGraph As String = ""

        For Each colaCla As String In clase
            codigoGraph += colaCla
        Next
        For Each colaAtri As String In atributos
            codigoGraph += colaAtri
        Next
        codigoGraph += "|"
        For Each colaMet As String In metodos
            codigoGraph += colaMet
        Next

        codigoGraph += Chr(34)

        For Each colaCol As String In color
            codigoGraph += colaCol
        Next

        codigoGraph += "];"

        tablagrafo.Rows.Add(codigoGraph)

    End Sub

    'para las asociaciones
    Public Sub llenarCola3()
        Dim bloqueasociacion As Boolean = False
        Dim contador As Integer = 0
        Dim asociacion As String = "asociacion"
        Dim agregacion As String = "agregacion"
        Dim composicion As String = "composicion"
        Dim herencia As String = "herencia"
        Dim asociacionsimple As String = "asociacionsimple"

      
        For Each row As DataGridViewRow In dgvbuena.Rows 'para recorrer todas las filas de mi tabla

            Try
                'para asociaciones
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = asociacion And dgvbuena.Rows(contador - 1).Cells(1).Value = "[" And dgvbuena.Rows(contador + 1).Cells(1).Value = "]" And dgvbuena.Rows(contador + 2).Cells(1).Value = "{" Then
                    bloqueasociacion = True
                    Console.WriteLine("-----------------BLOQUE ASOCIACION-----------------------------")
                End If

                If (bloqueasociacion = True) And row.Cells(1).Value = ":" Then
                    'para las relaciones entre clases, clase1:agregacion:clase2;
                    If dgvbuena.Rows(contador + 1).Cells(2).Value = "Palabra Reservada" And dgvbuena.Rows(contador + 2).Cells(1).Value = ":" And dgvbuena.Rows(contador + 4).Cells(1).Value = ";" Then
                        Console.WriteLine(dgvbuena.Rows(contador - 1).Cells(1).Value + "->" + dgvbuena.Rows(contador + 1).Cells(1).Value + "->" + dgvbuena.Rows(contador + 3).Cells(1).Value)

                        'para las agregaciones graphiz
                        If LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = agregacion Then
                            tablagrafo2.Rows.Add(Chr(34) & dgvbuena.Rows(contador - 1).Cells(1).Value & Chr(34) & " -> " & Chr(34) & dgvbuena.Rows(contador + 3).Cells(1).Value & Chr(34) & " [arrowhead=" & Chr(34) & "odiamond" & Chr(34) & "]")
                        ElseIf LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = composicion Then
                            tablagrafo2.Rows.Add(Chr(34) & dgvbuena.Rows(contador - 1).Cells(1).Value & Chr(34) & " -> " & Chr(34) & dgvbuena.Rows(contador + 3).Cells(1).Value & Chr(34) & " [arrowhead=" & Chr(34) & "diamond" & Chr(34) & "]")
                        ElseIf LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = herencia Then
                            tablagrafo2.Rows.Add(Chr(34) & dgvbuena.Rows(contador - 1).Cells(1).Value & Chr(34) & " -> " & Chr(34) & dgvbuena.Rows(contador + 3).Cells(1).Value & Chr(34) & " [arrowhead=" & Chr(34) & "onormal" & Chr(34) & "]")
                        ElseIf LCase(dgvbuena.Rows(contador + 1).Cells(1).Value) = asociacionsimple Then
                            tablagrafo2.Rows.Add(Chr(34) & dgvbuena.Rows(contador - 1).Cells(1).Value & Chr(34) & " -> " & Chr(34) & dgvbuena.Rows(contador + 3).Cells(1).Value & Chr(34) & " [arrowhead=" & Chr(34) & "none" & Chr(34) & "]")
                        End If

                        If dgvbuena.Rows(contador + 5).Cells(1).Value = "}" Then
                            bloqueasociacion = False
                            Console.WriteLine("---------------SE CERRO BLOQUE ASOCIACION--------------")
                        End If
                        'para que comentario corresponde a la clase, clase1:comentario1;
                    ElseIf dgvbuena.Rows(contador - 1).Cells(2).Value = "Identificador" And dgvbuena.Rows(contador + 1).Cells(2).Value = "Identificador" And dgvbuena.Rows(contador + 2).Cells(1).Value = ";" Then
                        Console.WriteLine(dgvbuena.Rows(contador - 1).Cells(1).Value + "->" + dgvbuena.Rows(contador + 1).Cells(1).Value)

                        Dim contadore As Integer = 0

                        For Each row2 As DataGridViewRow In tablagrafo3.Rows
                            If row2.Cells(0).Value = dgvbuena.Rows(contador + 1).Cells(1).Value Then
                                tablagrafo4.Rows.Add(Chr(34) & dgvbuena.Rows(contador - 1).Cells(1).Value & Chr(34) & " -> " & Chr(34) & tablagrafo3.Rows(contadore).Cells(1).Value & Chr(34) & " [style=" & Chr(34) & "dashed" & Chr(34) & "]")
                            End If
                            contadore += 1
                        Next

                        If dgvbuena.Rows(contador + 3).Cells(1).Value = "}" Then
                            bloqueasociacion = False
                            Console.WriteLine("---------------SE CERRO BLOQUE ASOCIACION--------------")
                        End If
                    End If

                End If

            Catch ex As Exception

            End Try
            contador = contador + 1
        Next

    End Sub

    Function colores(ByVal R As Integer, ByVal G As Integer, ByVal B As Integer) As String
        Dim hexa As String = "#"
        Dim hexValue As String = String.Format("{0}{1}{2}", R.ToString("X").PadLeft(2, "0"), G.ToString("X").PadLeft(2, "0"), B.ToString("X").PadLeft(2, "0"))
        hexa = hexa + hexValue
        'MessageBox.Show(hexValue)
        Return hexa
    End Function

    Public Sub analisisSintactivo()

        Dim contador As Integer = 0 'contador para ver en que fila me encuentro

        'variables para verificar que datos tengo
        Dim clase As String = "clase"
        Dim nombre As String = "nombre"
        Dim atributos As String = "atributos"
        Dim metodos As String = "metodos"
        Dim asociacion As String = "asociacion"
        Dim agregacion As String = "agregacion"
        Dim composicion As String = "composicion"
        Dim herencia As String = "herencia"
        Dim asociacionsimple As String = "asociacionsimple"
        Dim color As String = "color"
        Dim comentario As String = "comentario"
        Dim texto As String = "texto"
        
        For Each row As DataGridViewRow In dgvbuena.Rows 'para recorrer todas las filas de mi tabla

            Try
                'PARA VER SI UNA PALABRA RESERVADA CLASE ESTA DE ESTA MANERA [cLase] si esta entre corchetes
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = clase Then
                    If dgvbuena.Rows(contador - 1).Cells(1).Value <> "[" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que abre", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)

                    End If
                    If dgvbuena.Rows(contador + 1).Cells(1).Value <> "]" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que cierra", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If
                End If

                'PARA VER SI UNA PALABRA RESERVADA METODOS ESTA DE ESTA MANERA [MetodOs] si esta entre corchetes
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = metodos Then
                    If dgvbuena.Rows(contador - 1).Cells(1).Value <> "[" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que abre", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)

                    End If
                    If dgvbuena.Rows(contador + 1).Cells(1).Value <> "]" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que cierra", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If

                End If

                'PARA VER SI UNA PALABRA RESERVADA Nombre ESTA DE ESTA MANERA [Nombre] si esta entre corchetes
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = nombre Then
                    If dgvbuena.Rows(contador - 1).Cells(1).Value <> "[" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que abre", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)

                    End If
                    If dgvbuena.Rows(contador + 1).Cells(1).Value <> "]" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que cierra", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If

                End If

                'PARA VER SI UNA PALABRA RESERVADA ATRIBUTOS ESTA DE ESTA MANERA [atributos] si esta entre corchetes
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = atributos Then
                    If dgvbuena.Rows(contador - 1).Cells(1).Value <> "[" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que abre", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)

                    End If
                    If dgvbuena.Rows(contador + 1).Cells(1).Value <> "]" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que cierra", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If

                End If

                'PARA VER SI UNA PALABRA RESERVADA ASOCIACION ESTA DE ESTA MANERA [asociacion] si esta entre corchetes
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = asociacion Then
                    If dgvbuena.Rows(contador - 1).Cells(1).Value <> "[" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que abre", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)

                    End If
                    If dgvbuena.Rows(contador + 1).Cells(1).Value <> "]" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que cierra", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If

                End If

                'PARA VER SI UNA PALABRA RESERVADA COMENTARIO ESTA DE ESTA MANERA [comentario] si esta entre corchetes
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = comentario Then
                    If dgvbuena.Rows(contador - 1).Cells(1).Value <> "[" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que abre", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)

                    End If
                    If dgvbuena.Rows(contador + 1).Cells(1).Value <> "]" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que cierra", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If

                End If

                'PARA VER SI UNA PALABRA RESERVADA TEXTO ESTA DE ESTA MANERA [texto] si esta entre corchetes
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = texto Then
                    If dgvbuena.Rows(contador - 1).Cells(1).Value <> "[" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que abre", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)

                    End If
                    If dgvbuena.Rows(contador + 1).Cells(1).Value <> "]" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que cierra", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If

                End If

                'PARA VER SI UNA PALABRA RESERVADA COLOR ESTA DE ESTA MANERA [color] si esta entre corchetes
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = color Then
                    If dgvbuena.Rows(contador - 1).Cells(1).Value <> "[" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que abre", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)

                    End If
                    If dgvbuena.Rows(contador + 1).Cells(1).Value <> "]" Then
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba corchete que cierra", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If

                End If

                'PARA VER SI UNA CLASE ESTA TIENE CORCHETE DE APERTURA {
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = clase Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "]" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value <> "{" Then
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba llave de apertura", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)
                        End If
                    End If
                End If

                'PARA VER SI UN BLOQUE ATRIBUTOS TIENE CORCHETE DE APERTURA {
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = atributos Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "]" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value <> "{" Then
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba llave de apertura", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)
                        End If
                    End If
                End If

                'PARA VER SI UN BLOQUE METODOS TIENE CORCHETE DE APERTURA {
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = metodos Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "]" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value <> "{" Then
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba llave de apertura", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)
                        End If
                    End If
                End If

                'PARA VER SI UN BLOQUE COMENTARIO TIENE CORCHETE DE APERTURA {
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = comentario Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "]" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value <> "{" Then
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba llave de apertura", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)
                        End If
                    End If
                End If

                'PARA VER SI UN BLOQUE METODOS TIENE CORCHETE DE APERTURA {
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = asociacion Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "]" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value <> "{" Then
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba llave de apertura", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)
                        End If
                    End If
                End If

                'PARA VER SI DESPUES DE ESTA ESTRUCTURA [COLOR]=2,2,3;
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = color Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "]" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value = "=" Then
                            If dgvbuena.Rows(contador + 3).Cells(2).Value = "Numerico" Then
                                If dgvbuena.Rows(contador + 4).Cells(1).Value = "," Then
                                    If dgvbuena.Rows(contador + 5).Cells(2).Value = "Numerico" Then
                                        If dgvbuena.Rows(contador + 6).Cells(1).Value = "," Then
                                            If dgvbuena.Rows(contador + 7).Cells(2).Value = "Numerico" Then
                                                If dgvbuena.Rows(contador + 8).Cells(1).Value = ";" Then




                                                Else
                                                    contador_error_2 = contador_error_2 + 1
                                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 8).Cells(1).Value, "Se esperaba Signo ';'", dgvbuena.Rows(contador + 8).Cells(3).Value, dgvbuena.Rows(contador + 8).Cells(4).Value)
                                                End If
                                            Else
                                                contador_error_2 = contador_error_2 + 1
                                                dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 7).Cells(1).Value, "Se Esperaba Valor Numerico", dgvbuena.Rows(contador + 7).Cells(3).Value, dgvbuena.Rows(contador + 7).Cells(4).Value)
                                            End If
                                        Else
                                            contador_error_2 = contador_error_2 + 1
                                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 6).Cells(1).Value, "Se esperaba signo ','", dgvbuena.Rows(contador + 6).Cells(3).Value, dgvbuena.Rows(contador + 6).Cells(4).Value)
                                        End If
                                    Else
                                        contador_error_2 = contador_error_2 + 1
                                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 5).Cells(1).Value, "Se Esperaba Valor Numerico", dgvbuena.Rows(contador + 5).Cells(3).Value, dgvbuena.Rows(contador + 5).Cells(4).Value)
                                    End If
                                Else
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 4).Cells(1).Value, "Se esperaba signo ','", dgvbuena.Rows(contador + 4).Cells(3).Value, dgvbuena.Rows(contador + 4).Cells(4).Value)
                                End If
                            Else
                                contador_error_2 = contador_error_2 + 1
                                dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 3).Cells(1).Value, "Se Esperaba Valor Numerico", dgvbuena.Rows(contador + 3).Cells(3).Value, dgvbuena.Rows(contador + 3).Cells(4).Value)

                            End If
                        Else
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba signo '='", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)
                        End If
                    End If
                End If

                'PARA VER SI ESTA ESTA ESTRUCTURA [NOMBRE]=progra;
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = nombre Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "]" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value = "=" Then
                            If dgvbuena.Rows(contador + 3).Cells(2).Value = "Identificador" Then
                                If dgvbuena.Rows(contador + 4).Cells(1).Value = ";" Then

                                Else
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 4).Cells(1).Value, "Se esperaba signo ';'", dgvbuena.Rows(contador + 4).Cells(3).Value, dgvbuena.Rows(contador + 4).Cells(4).Value)
                                End If
                            Else
                                contador_error_2 = contador_error_2 + 1
                                dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 3).Cells(1).Value, "Se esperaba Identificador", dgvbuena.Rows(contador + 3).Cells(3).Value, dgvbuena.Rows(contador + 3).Cells(4).Value)
                            End If
                        Else
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba signo '='", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)
                        End If
                    End If
                End If

                'PARA VER SI [TEXTO] ESTA BIEN ESTRUCTURADO: [TEXTO] = COMENTARIO ENTRE COMILLAS
                If row.Cells(2).Value = "Palabra Reservada" And LCase(row.Cells(1).Value) = texto Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "]" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value = "=" Then
                            If dgvbuena.Rows(contador + 3).Cells(1).Value = Chr(34) Then
                                If dgvbuena.Rows(contador + 4).Cells(2).Value = "Identificador" Then
                                    If dgvbuena.Rows(contador + 5).Cells(1).Value = Chr(34) Then
                                        If dgvbuena.Rows(contador + 6).Cells(1).Value = ";" Then

                                        Else
                                            contador_error_2 = contador_error_2 + 1
                                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 6).Cells(1).Value, "Se esperaba Signo ';' ", dgvbuena.Rows(contador + 6).Cells(3).Value, dgvbuena.Rows(contador + 6).Cells(4).Value)
                                        End If
                                    Else
                                        contador_error_2 = contador_error_2 + 1
                                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 5).Cells(1).Value, "Se esperaba " & Chr(34), dgvbuena.Rows(contador + 5).Cells(3).Value, dgvbuena.Rows(contador + 5).Cells(4).Value)
                                    End If
                                Else
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 4).Cells(1).Value, "Se esperaba Identificador", dgvbuena.Rows(contador + 4).Cells(3).Value, dgvbuena.Rows(contador + 4).Cells(4).Value)
                                End If
                            Else
                                contador_error_2 = contador_error_2 + 1
                                dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 3).Cells(1).Value, "Se esperaba " & Chr(34), dgvbuena.Rows(contador + 3).Cells(3).Value, dgvbuena.Rows(contador + 3).Cells(4).Value)
                            End If
                        Else
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba signo '='", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)

                        End If
                    End If
                End If

                'PARA VER SI LA ESTRUCTURA DE UN ATRIBUTO O METODO ESTA BIEN ESCRITA (+)nombre:string;
                If row.Cells(2).Value = "Visibilidad" And (LCase(row.Cells(1).Value) = "(+)" Or LCase(row.Cells(1).Value) = "(-)" Or LCase(row.Cells(1).Value) = "(#)") Then
                    If dgvbuena.Rows(contador + 1).Cells(2).Value = "Identificador" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value = ":" Then
                            If dgvbuena.Rows(contador + 3).Cells(2).Value = "Identificador" Then
                                If dgvbuena.Rows(contador + 4).Cells(1).Value = ";" Then

                                    If dgvbuena.Rows(contador + 5).Cells(2).Value = "Visibilidad" Then
                                    ElseIf dgvbuena.Rows(contador + 5).Cells(1).Value = "}" Then
                                    Else
                                        contador_error_2 = contador_error_2 + 1
                                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 5).Cells(1).Value, "Se esperaba visivilidad o '{'", dgvbuena.Rows(contador + 5).Cells(3).Value, dgvbuena.Rows(contador + 5).Cells(4).Value)

                                    End If



                                Else
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 4).Cells(1).Value, "Se esperaba signo ';'", dgvbuena.Rows(contador + 4).Cells(3).Value, dgvbuena.Rows(contador + 4).Cells(4).Value)
                                End If
                            Else
                                contador_error_2 = contador_error_2 + 1
                                dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 3).Cells(1).Value, "Se esperaba Identificador (Tipo de dato o Tipo de Retorno)", dgvbuena.Rows(contador + 3).Cells(3).Value, dgvbuena.Rows(contador + 3).Cells(4).Value)
                            End If
                        ElseIf dgvbuena.Rows(contador + 2).Cells(1).Value = ";" Then



                        Else
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba signo ':' o ';'", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)
                        End If
                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba Identificador (Nombre del atributo)", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                    End If
                End If

                'PARA VER SI LA ESTRUCTURA DE RELACION DE CLASES ESTA BIEN: CLASE1:AGREGACION:CLASE;
                If row.Cells(2).Value = "Palabra Reservada" And (LCase(row.Cells(1).Value) = agregacion Or LCase(row.Cells(1).Value) = composicion Or LCase(row.Cells(1).Value) = herencia Or LCase(row.Cells(1).Value) = asociacionsimple) Then
                    If dgvbuena.Rows(contador - 2).Cells(2).Value = "Identificador" Then
                        If dgvbuena.Rows(contador - 1).Cells(1).Value = ":" Then
                            If dgvbuena.Rows(contador + 1).Cells(1).Value = ":" Then
                                If dgvbuena.Rows(contador + 2).Cells(2).Value = "Identificador" Then
                                    If dgvbuena.Rows(contador + 3).Cells(1).Value = ";" Then




                                    Else
                                        contador_error_2 = contador_error_2 + 1
                                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 3).Cells(1).Value, "Se esperaba Signo ';'", dgvbuena.Rows(contador + 3).Cells(3).Value, dgvbuena.Rows(contador + 3).Cells(4).Value)
                                    End If
                                Else
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 2).Cells(1).Value, "Se esperaba Identificador (Nombre de Clase)", dgvbuena.Rows(contador + 2).Cells(3).Value, dgvbuena.Rows(contador + 2).Cells(4).Value)
                                End If
                            Else
                                contador_error_2 = contador_error_2 + 1
                                dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba Signo ':'", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                            End If
                        Else
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador - 1).Cells(1).Value, "Se esperaba Signo ':'", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                        End If
                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador - 2).Cells(1).Value, "Se esperaba Identificador (Nombre de Clase)", dgvbuena.Rows(contador - 2).Cells(3).Value, dgvbuena.Rows(contador - 2).Cells(4).Value)

                    End If

                End If

                'PARA VER SI LA ESTRUCTURA DE RELACION DE CLASES ESTA BIEN: CLASE1:COMENTARIO;
                If row.Cells(2).Value = "Identificador" Then
                    If dgvbuena.Rows(contador + 2).Cells(2).Value = "Identificador" Then
                        If dgvbuena.Rows(contador - 1).Cells(1).Value = ";" Or dgvbuena.Rows(contador - 1).Cells(1).Value = "{" Then
                            If dgvbuena.Rows(contador + 1).Cells(1).Value = ":" Then
                                If dgvbuena.Rows(contador + 3).Cells(1).Value = ";" Then
                                Else
                                    contador_error_2 = contador_error_2 + 1
                                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 3).Cells(1).Value, "Se esperaba ';'", dgvbuena.Rows(contador + 3).Cells(3).Value, dgvbuena.Rows(contador + 3).Cells(4).Value)

                                End If

                            Else
                                contador_error_2 = contador_error_2 + 1
                                dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba ':'", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)

                            End If

                        End If

                    End If
                End If

                'PARA LOS DATOS QUE VAN ANTES Y DESPUES DE LA LLAVE {
                If row.Cells(1).Value = "{" And (dgvbuena.Rows(contador - 1).Cells(1).Value <> "]") Then
                    contador_error_2 = contador_error_2 + 1
                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador - 1).Cells(1).Value, "Lexema sin sentido", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                End If

                If row.Cells(1).Value = "{" Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "[" Then
                    ElseIf dgvbuena.Rows(contador + 1).Cells(2).Value = "Visibilidad" Then
                    ElseIf dgvbuena.Rows(contador + 1).Cells(2).Value = "Identificador" Then
                        If dgvbuena.Rows(contador + 2).Cells(1).Value = ":" Then
                        Else
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Lexema sin sentido", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                        End If
                    End If
                End If

                'DESPUES DE LLAVE CUANDO ABRE ATRIBUTO
                If row.Cells(1).Value = "{" And LCase(dgvbuena.Rows(contador - 2).Cells(1).Value) = atributos And (dgvbuena.Rows(contador + 1).Cells(2).Value <> "Visibilidad") Then
                    contador_error_2 = contador_error_2 + 1
                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Lexema sin sentido", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                End If

                'DESPUES DE LLAVE CUANDO ABRE METODO
                If row.Cells(1).Value = "{" And LCase(dgvbuena.Rows(contador - 2).Cells(1).Value) = metodos And (dgvbuena.Rows(contador + 1).Cells(2).Value <> "Visibilidad") Then
                    contador_error_2 = contador_error_2 + 1
                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Lexema sin sentido", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                End If

                'DESPUES DE LLAVE CUANDO ABRE COMENTARIO
                If row.Cells(1).Value = "{" And LCase(dgvbuena.Rows(contador - 2).Cells(1).Value) = comentario And dgvbuena.Rows(contador + 1).Cells(1).Value <> "[" Then
                    contador_error_2 = contador_error_2 + 1
                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Lexema sin sentido", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                End If

                'DESPUES DE LLAVE CUANDO ABRE ASOCIACION
                If row.Cells(1).Value = "{" And LCase(dgvbuena.Rows(contador - 2).Cells(1).Value) = asociacion And dgvbuena.Rows(contador + 1).Cells(2).Value <> "Identificador" Then
                    contador_error_2 = contador_error_2 + 1
                    dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Lexema sin sentido", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                End If

                'PARA VER SI HAY DOS IDENTIFICADORES SEGUIDOS
                If contador > 0 And contador < (dgvbuena.Rows.Count - 3) Then
                    If row.Cells(2).Value = "Identificador" Then
                        If dgvbuena.Rows(contador + 1).Cells(2).Value = "Identificador" Then
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se esperaba una instruccion", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)

                        End If
                    End If
                End If

                'PARA LOS DATOS QUE VAN ANTES Y DESPUES DE LA LLAVE }
                If row.Cells(1).Value = "}" Then
                    If dgvbuena.Rows(contador + 1).Cells(1).Value = "[" Then
                    ElseIf dgvbuena.Rows(contador + 1).Cells(1).Value = "}" Then
                    ElseIf dgvbuena.Rows(contador + 1).Cells(1).Value = " " Then
                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Lexema sin sentido", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                    End If

                End If

                If row.Cells(1).Value = "}" Then
                    If dgvbuena.Rows(contador - 1).Cells(1).Value = ";" Then
                    ElseIf dgvbuena.Rows(contador - 1).Cells(1).Value = "}" Then
                    ElseIf dgvbuena.Rows(contador - 1).Cells(1).Value = " " Then
                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador - 1).Cells(1).Value, "Lexema sin sentido", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If

                End If

                'PARA LOS DATOS QUE VAN ANTES Y DESPUES DE LA LLAVE [
                If row.Cells(1).Value = "[" Then
                    If dgvbuena.Rows(contador + 1).Cells(2).Value = "Palabra Reservada" Then

                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador + 1).Cells(1).Value, "Se Esperaba Palabra Reservada", dgvbuena.Rows(contador + 1).Cells(3).Value, dgvbuena.Rows(contador + 1).Cells(4).Value)
                    End If

                End If

                If row.Cells(1).Value = "]" Then
                    If dgvbuena.Rows(contador - 1).Cells(2).Value = "Palabra Reservada" Then

                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador - 1).Cells(1).Value, "Se Esperaba Palabra Reservada", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                    End If

                End If

                If row.Cells(1).Value.ToString().Contains("#") Or row.Cells(1).Value.ToString().Contains("+") Or row.Cells(1).Value.ToString().Contains("-") Then
                    If row.Cells(2).Value = "Visibilidad" Then
                        If dgvbuena.Rows(contador).Cells(1).Value = "(+)" Or dgvbuena.Rows(contador).Cells(1).Value = "(-)" Or dgvbuena.Rows(contador).Cells(1).Value = "(#)" Then
                        Else
                            contador_error_2 = contador_error_2 + 1
                            dgverror2.Rows.Add(contador_error_2, dgvbuena.Rows(contador).Cells(1).Value, "Simbolo de Visibilidad mal escrito", dgvbuena.Rows(contador - 1).Cells(3).Value, dgvbuena.Rows(contador - 1).Cells(4).Value)
                        End If
                    End If
                End If


            Catch ex As ArgumentOutOfRangeException
                'MessageBox.Show("Fuera de intervalo")
            End Try

            contador += 1 'contador aumenta en 1
        Next


    End Sub

    Public Sub erroresSintacticosLlaves()

        Dim contador As Integer = 0
        For Each row As DataGridViewRow In dgvbuena.Rows 'para recorrer todas las filas de mi tabla

            Try
                'PARA VER SI UNA PALABRA RESERVADA CLASE ESTA DE ESTA MANERA [cLase] si esta entre corchetes
                If row.Cells(1).Value = "{" And LCase(dgvbuena.Rows(contador - 2).Cells(1).Value) = "clase" Then
                    tablasintacticallaves.Rows.Add(dgvbuena.Rows(contador - 2).Cells(1).Value, row.Cells(1).Value, dgvbuena.Rows(contador).Cells(3).Value, dgvbuena.Rows(contador).Cells(4).Value)
                ElseIf row.Cells(1).Value = "{" And LCase(dgvbuena.Rows(contador - 2).Cells(1).Value) = "atributos" Then
                    tablasintacticallaves.Rows.Add(dgvbuena.Rows(contador - 2).Cells(1).Value, row.Cells(1).Value, dgvbuena.Rows(contador).Cells(3).Value, dgvbuena.Rows(contador).Cells(4).Value)
                ElseIf row.Cells(1).Value = "{" And LCase(dgvbuena.Rows(contador - 2).Cells(1).Value) = "metodos" Then
                    tablasintacticallaves.Rows.Add(dgvbuena.Rows(contador - 2).Cells(1).Value, row.Cells(1).Value, dgvbuena.Rows(contador).Cells(3).Value, dgvbuena.Rows(contador).Cells(4).Value)
                ElseIf row.Cells(1).Value = "{" And LCase(dgvbuena.Rows(contador - 2).Cells(1).Value) = "comentario" Then
                    tablasintacticallaves.Rows.Add(dgvbuena.Rows(contador - 2).Cells(1).Value, row.Cells(1).Value, dgvbuena.Rows(contador).Cells(3).Value, dgvbuena.Rows(contador).Cells(4).Value)
                ElseIf row.Cells(1).Value = "{" And LCase(dgvbuena.Rows(contador - 2).Cells(1).Value) = "asociacion" Then
                    tablasintacticallaves.Rows.Add(dgvbuena.Rows(contador - 2).Cells(1).Value, row.Cells(1).Value, dgvbuena.Rows(contador).Cells(3).Value, dgvbuena.Rows(contador).Cells(4).Value)
                ElseIf row.Cells(1).Value = "}" Then
                    tablasintacticallaves.Rows.Add("-", row.Cells(1).Value, dgvbuena.Rows(contador).Cells(3).Value, dgvbuena.Rows(contador).Cells(4).Value)
                End If

            Catch ex As Exception

            End Try
            contador = contador + 1
        Next


    End Sub

    Public Sub erroresSintactivosLlaves2()
        Dim contador As Integer = 0
        For Each row As DataGridViewRow In tablasintacticallaves.Rows 'para recorrer todas las filas de mi tabla

            Try
                'PARA VER SI UNA PALABRA RESERVADA CLASE ESTA DE ESTA MANERA [cLase] si esta entre corchetes
                If LCase(row.Cells(0).Value) = "clase" Then
                    If LCase(tablasintacticallaves.Rows(contador + 6).Cells(0).Value) = "clase" Or LCase(tablasintacticallaves.Rows(contador + 6).Cells(0).Value) = "asociacion" Or LCase(tablasintacticallaves.Rows(contador + 6).Cells(0).Value) = "comentario" Then
                    ElseIf LCase(tablasintacticallaves.Rows(contador + 4).Cells(0).Value) = "clase" Or LCase(tablasintacticallaves.Rows(contador + 4).Cells(0).Value) = "asociacion" Or LCase(tablasintacticallaves.Rows(contador + 4).Cells(0).Value) = "comentario" Then
                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, tablasintacticallaves.Rows(contador).Cells(1).Value, "Falta llave de cierre de Bloque Clase, o bien llave de cierre de Atributo o Metodo perteneciente a Dicha Bloque", tablasintacticallaves.Rows(contador).Cells(2).Value, tablasintacticallaves.Rows(contador).Cells(3).Value)
                    End If
                End If

                If LCase(row.Cells(0).Value) = "comentario" Then
                    If tablasintacticallaves.Rows(contador + 1).Cells(1).Value = "}" Then
                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, tablasintacticallaves.Rows(contador).Cells(1).Value, "Falta llave de cierre de Bloque Comentario", tablasintacticallaves.Rows(contador).Cells(2).Value, tablasintacticallaves.Rows(contador).Cells(3).Value)
                    End If

                End If

                If LCase(row.Cells(0).Value) = "asociacion" Then
                    If tablasintacticallaves.Rows(contador + 1).Cells(1).Value = "}" Then
                    Else
                        contador_error_2 = contador_error_2 + 1
                        dgverror2.Rows.Add(contador_error_2, tablasintacticallaves.Rows(contador).Cells(1).Value, "Falta llave de cierre de Bloque Asociacion", tablasintacticallaves.Rows(contador).Cells(2).Value, tablasintacticallaves.Rows(contador).Cells(3).Value)
                    End If
                End If


            Catch ex As Exception

            End Try
            contador = contador + 1
        Next
    End Sub

    Public Sub pintarErroresSintactivos()

        Dim examinar_ascii_1 As Integer = 0
        Dim examinar_1 As String = ""
        Dim contador As Integer = 0 'cuenta las filas con el asci
        Dim sumaDesde As Integer = 0
        Dim sumaHasta As Integer = 0
        Dim contador2 As Integer = 0 'para dgv
        Dim fila As Integer = 0
        Dim columna As Integer = 0
        Dim tamanio As Integer = 0
        Dim texto_1 As String = areaAnalizar.Text.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter

        For Each row As DataGridViewRow In dgverror2.Rows

            Try
                fila = Convert.ToInt32(dgverror2.Rows(contador2).Cells(4).Value) 'valor de la fila de tablas de erroes sintactivos
                columna = Convert.ToInt32(dgverror2.Rows(contador2).Cells(3).Value) 'valor de la columna de tablas de erroes sintactivos
            Catch ex As Exception

            End Try
            '------------------------------------------------------
            For puntero_1 As Integer = 0 To areaAnalizar.TextLength - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
                examinar_1 = texto_1(puntero_1)
                examinar_ascii_1 = Asc(examinar_1)

                If examinar_ascii_1 = 10 Then  'salto de linea'
                    contador = contador + 1 'cada vez que hay salto de linea me aumenta en 1
                    If contador = fila - 1 Then
                        'MessageBox.Show(contador)
                        'MessageBox.Show(puntero_1)
                        'MessageBox.Show(tamanio)
                        'MessageBox.Show(puntero_1 - tamanio)
                        tamanio = row.Cells(1).Value.ToString().Length 'tamanio de mi error
                        sumaDesde = puntero_1 + columna - tamanio 'sera desde donde va burcar, puntero son todos los caracteres que hay, hasta que llega a la fila de nuestro error, se le suma la culumna donde esta el error y se le resta el tamanio de nuestro error
                        sumaHasta = puntero_1 + columna 'sera hasta donde va buscar
                        For q = sumaDesde To sumaHasta
                            If LCase(areaAnalizar.Text.Substring(q, row.Cells(1).Value.ToString().Length)) = LCase(row.Cells(1).Value) Then
                                areaAnalizar.Select(q, row.Cells(1).Value.ToString().Length)
                                areaAnalizar.SelectionColor = Color.Blue
                            End If
                        Next
                    End If
                End If
            Next
            '------------------------------------------------------------
            contador = 0
            contador2 = contador2 + 1
        Next
    End Sub

    Public Sub pintarErroresLexicos()

        Dim examinar_ascii_1 As Integer = 0
        Dim examinar_1 As String = ""
        Dim contador As Integer = 0 'cuenta las filas con el asci
        Dim sumaDesde As Integer = 0
        Dim sumaHasta As Integer = 0
        Dim contador2 As Integer = 0 'para dgv
        Dim fila As Integer = 0
        Dim columna As Integer = 0
        Dim tamanio As Integer = 0
        Dim texto_1 As String = areaAnalizar.Text.ToCharArray 'me sapara todos los caracter del area analizar para que vaya analizando caracter por caracter

        For Each row As DataGridViewRow In dgverror.Rows

            Try
                fila = Convert.ToInt32(dgverror.Rows(contador2).Cells(3).Value) 'valor de la fila de tablas de erroes sintactivos
                columna = Convert.ToInt32(dgverror.Rows(contador2).Cells(2).Value) 'valor de la columna de tablas de erroes sintactivos
            Catch ex As Exception

            End Try
            '------------------------------------------------------
            For puntero_1 As Integer = 0 To areaAnalizar.TextLength - 1 'ciclo para que vaya anilizando cada caracter y me de el codigo ascii
                examinar_1 = texto_1(puntero_1)
                examinar_ascii_1 = Asc(examinar_1)

                If examinar_ascii_1 = 10 Then  'salto de linea'
                    contador = contador + 1 'cada vez que hay salto de linea me aumenta en 1
                    If contador = fila - 1 Then
                        'MessageBox.Show(contador)
                        'MessageBox.Show(puntero_1)
                        'MessageBox.Show(tamanio)
                        'MessageBox.Show(puntero_1 - tamanio)
                        tamanio = row.Cells(1).Value.ToString().Length 'tamanio de mi error
                        sumaDesde = puntero_1 + columna - tamanio 'sera desde donde va burcar, puntero son todos los caracteres que hay, hasta que llega a la fila de nuestro error, se le suma la culumna donde esta el error y se le resta el tamanio de nuestro error
                        sumaHasta = puntero_1 + columna 'sera hasta donde va buscar
                        For q = sumaDesde To sumaHasta
                            If areaAnalizar.Text.Substring(q, row.Cells(1).Value.ToString().Length) = row.Cells(1).Value.ToString() Then
                                areaAnalizar.Select(q, row.Cells(1).Value.ToString().Length)
                                areaAnalizar.SelectionColor = Color.Blue
                            End If
                        Next
                    End If
                End If
            Next
            '------------------------------------------------------------
            contador = 0
            contador2 = contador2 + 1
        Next
    End Sub

    Public Sub graficar()
        Dim sw As New System.IO.StreamWriter("grafo.txt")
        sw.WriteLine(generarGrafo())
        sw.Close()
        Dim prog As VariantType
        prog = Shell("C:\Program Files (x86)\Graphviz2.38\bin\dot.exe -Tjpg grafo.txt -o grafo.jpg", 1)

    End Sub


    Public Function generarGrafo() As String
        Dim generar As String = ""

        generar += "digraph grafica{" & vbCrLf
        generar += "rankdir=LR;"
        generar += "size=" & Chr(34) & "16,5" & Chr(34)
        generar += "node [shape = record ]; "

        For Each row As DataGridViewRow In tablagrafo.Rows

            generar = generar + row.Cells(0).Value

        Next

        generar += vbCrLf

        For Each row2 As DataGridViewRow In tablagrafo2.Rows

            generar = generar + row2.Cells(0).Value

        Next

        For Each row3 As DataGridViewRow In tablagrafo4.Rows

            generar = generar + row3.Cells(0).Value

        Next

        generar += "}"
        Console.WriteLine(generar)
        Return generar
    End Function

    Public Sub abrirGrafo()
        Dim loPSI As New ProcessStartInfo
        Dim proceso As New Process
        loPSI.FileName = "grafo.jpg"

        Try
            proceso = Process.Start(loPSI)

        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try
    End Sub

    Public Sub MetodoHTML_bueno()


        Dim ii As Integer = 0
        Dim DireccionDocumento As String = My.Computer.FileSystem.SpecialDirectories.Desktop
        My.Computer.FileSystem.CreateDirectory(DireccionDocumento & "\Reportes")
        Dim Path As String = DireccionDocumento & "\Reportes\Reporte_Tokens.html"
        Dim Archivo As FileInfo = New FileInfo(Path)
        Dim Escribir As StreamWriter
        Escribir = File.CreateText(Path)
        Escribir.WriteLine("<html>")
        Escribir.WriteLine("<head><title>Reporte De Tokens</title></head>")
        Escribir.WriteLine("<body bgcolor=white text=Black background=FondoEj3.gif >")
        Escribir.WriteLine("<h1><center>Reporte Tokens</center></h1>")
        Escribir.WriteLine("<br>")
        Escribir.WriteLine("<center>")
        Escribir.WriteLine("<table border= 1 width= 500 bgcolor= SILVER>")
        Escribir.WriteLine("<tr>")
        Escribir.WriteLine("<th>No.</th>")
        Escribir.WriteLine("<th>Lexema</th>")
        Escribir.WriteLine("<th>Tipo</th>")
        Escribir.WriteLine("<th>Columna</th>")
        Escribir.WriteLine("<th>Linea</th>")

        Escribir.WriteLine("</tr>")
        Dim PosicionError As Integer = 0
        For Each row As DataGridViewRow In dgvbuena.Rows

            Escribir.WriteLine("<tr>")
            Escribir.WriteLine("<th>" & row.Cells(0).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(1).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(2).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(3).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(4).Value & "</th>")

            Escribir.WriteLine("</tr>")

        Next
        Escribir.WriteLine("</table><br>")
        Escribir.WriteLine("</center></body></html>")
        Escribir.Flush()
        Escribir.Close()

        Dim proceso As New System.Diagnostics.Process
        With proceso
            .StartInfo.FileName = Path
            .Start()
        End With


    End Sub

    Public Sub MetodoHTML_Errores()


        Dim ii As Integer = 0
        Dim DireccionDocumento As String = My.Computer.FileSystem.SpecialDirectories.Desktop
        My.Computer.FileSystem.CreateDirectory(DireccionDocumento & "\Reportes")
        Dim Path As String = DireccionDocumento & "\Reportes\Reporte_Errores.html"
        Dim Archivo As FileInfo = New FileInfo(Path)
        Dim Escribir As StreamWriter
        Escribir = File.CreateText(Path)

        Escribir.WriteLine("<html>")
        Escribir.WriteLine("<head><title>Reporte De Errores</title></head>")
        Escribir.WriteLine("<body bgcolor=white text=Black background=FondoEj3.gif >")

        'para tabla errores
        Escribir.WriteLine("<h1><center>Reporte Errores Lexicos</center></h1>")
        Escribir.WriteLine("<br>")
        Escribir.WriteLine("<center>")
        Escribir.WriteLine("<table border= 1 width= 500 bgcolor= SILVER>")
        Escribir.WriteLine("<tr>")
        Escribir.WriteLine("<th>No.</th>")
        Escribir.WriteLine("<th>Error</th>")
        Escribir.WriteLine("<th>Columna</th>")
        Escribir.WriteLine("<th>Fila</th>")
        Escribir.WriteLine("<th>Descripcion</th>")

        Escribir.WriteLine("</tr>")

        For Each row As DataGridViewRow In dgverror.Rows

            Escribir.WriteLine("<tr>")
            Escribir.WriteLine("<th>" & row.Cells(0).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(1).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(2).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(3).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(4).Value & "</th>")

            Escribir.WriteLine("</tr>")

        Next
        Escribir.WriteLine("</table><br>")

        Escribir.WriteLine("<p></p>")

        'para la tabla otros errores
        Escribir.WriteLine("<h1><center>Reporte Errores Sintacticos</center></h1>")
        Escribir.WriteLine("<br>")
        Escribir.WriteLine("<center>")
        Escribir.WriteLine("<table border= 1 width= 500 bgcolor= SILVER>")
        Escribir.WriteLine("<tr>")
        Escribir.WriteLine("<th>No.</th>")
        Escribir.WriteLine("<th>Error</th>")
        Escribir.WriteLine("<th>Descripcion</th>")
        Escribir.WriteLine("<th>Columna</th>")
        Escribir.WriteLine("<th>Fila</th>")

        Escribir.WriteLine("</tr>")

        For Each row As DataGridViewRow In dgverror2.Rows

            Escribir.WriteLine("<tr>")
            Escribir.WriteLine("<th>" & row.Cells(0).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(1).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(2).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(3).Value & "</th>")
            Escribir.WriteLine("<th>" & row.Cells(4).Value & "</th>")

            Escribir.WriteLine("</tr>")

        Next
        Escribir.WriteLine("</table><br>")

        Escribir.WriteLine("</center></body></html>")
        Escribir.Flush()
        Escribir.Close()

        Dim proceso As New System.Diagnostics.Process
        With proceso
            .StartInfo.FileName = Path
            .Start()
        End With


    End Sub

    Private Sub bttanalizar_Click(sender As Object, e As EventArgs) Handles bttanalizar.Click
        
        iniciarAnalisis()
        
    End Sub

    Public Sub iniciarAnalisis()

        'limpiar tablas para que me genere nuevo grafo
        tablagrafo.Rows.Clear()
        tablagrafo2.Rows.Clear()
        tablagrafo3.Rows.Clear()
        tablagrafo4.Rows.Clear()
        tablasintacticallaves.Rows.Clear()
        dgvatributosMetodos.Rows.Clear()
        dgvbuena.Rows.Clear()
        dgverror.Rows.Clear()
        dgverror2.Rows.Clear()

        'para que el contador se reinicie
        contador_token = 0
        contador_error_1 = 0
        contador_error_2 = 0
        contadorLlaves = 0
        contadorLlaves2 = 0

        metodo_analizar()
        'agrego esto para que me puede leer la tabla completa
        dgvbuena.Rows.Add(" ", " ", " ", " ", " ")
        dgvbuena.Rows.Add(" ", " ", " ", " ", " ")
        dgvbuena.Rows.Add(" ", " ", " ", " ", " ")
        dgvbuena.Rows.Add(" ", " ", " ", " ", " ")


        llenarCola()
        llenarCola3()

        analisisSintactivo()

        erroresSintacticosLlaves()
        erroresSintactivosLlaves2()

        pintarLetras()
        pintarErroresLexicos()
        pintarErroresSintactivos()
        
        MetodoHTML_bueno()
        MetodoHTML_Errores()
        'contarLlaves()

        If dgverror.Rows.Count >= 2 Or dgverror2.Rows.Count >= 2 Then
            MessageBox.Show("No se puede generar GRAFO, Por errorres Lexicos o Sintacticos.")
        Else
            graficar()
            abrirGrafo()
        End If


    End Sub

    Private Sub bttlimpiar_Click(sender As Object, e As EventArgs) Handles bttlimpiar.Click

        For i = 128 To 255

            If Chr(i) = "ñ" Or Chr(i) = "Ñ" Then
                MessageBox.Show(i)
            End If

        Next

        
        areaAnalizar.Text = ""
        tablagrafo.Rows.Clear()
        tablagrafo2.Rows.Clear()
        tablagrafo3.Rows.Clear()
        tablagrafo4.Rows.Clear()
        tablasintacticallaves.Rows.Clear()
        dgvatributosMetodos.Rows.Clear()
        dgvbuena.Rows.Clear()
        dgverror.Rows.Clear()
        dgverror2.Rows.Clear()

    End Sub


    Public Sub metodo_abrir()
        Dim OpenFileDialog1 As New OpenFileDialog()
        Dim Alltext As String = "", LineOfText As String = ""
        OpenFileDialog1.Filter = "Text files (*.lfp)|*.lfp"
        OpenFileDialog1.ShowDialog()
        If OpenFileDialog1.FileName <> "" Then
            Try
                FileOpen(1, OpenFileDialog1.FileName, OpenMode.Input)
                Do Until EOF(1)
                    LineOfText = LineInput(1)
                    Alltext = Alltext & LineOfText & vbCrLf
                Loop
                'ArchivoToolStripMenuItem.Text = OpenFileDialog1.FileName'
                areaAnalizar.Text = vbLf & Alltext
                areaAnalizar.Enabled = True
                'ToolStripMenuItem.Close.Enabled = True'

            Catch ex As Exception
                MsgBox("Error")
            Finally
                FileClose(1)
            End Try
        End If
    End Sub

    Public Sub metodo_guardar()
        Dim Save As New SaveFileDialog()
        Dim myStreamWriter As System.IO.StreamWriter
        Save.Filter = "Documento LFP [*.LFP*]|*.LFP*|Todos los archivos [*,*]|*,*"
        Save.CheckPathExists = True
        Save.Title = "Guardar"
        Save.ShowDialog(Me)
        Try
            myStreamWriter = System.IO.File.AppendText(Save.FileName & ".lfp")
            myStreamWriter.Write(areaAnalizar.Text)
            myStreamWriter.Flush()
        Catch ex As Exception

        End Try
    End Sub

    Public Sub metodo_guardarcomo()
        Dim Save As New SaveFileDialog()
        Dim myStreamWriter As System.IO.StreamWriter
        Save.Filter = "Documento LFP [*.lfprec*]|*.lfprec*|Todos los archivos [*,*]|*,*"
        Save.CheckPathExists = True
        Save.Title = "Guardar"
        Save.ShowDialog(Me)
        Try
            myStreamWriter = System.IO.File.AppendText(Save.FileName & ".lfp")
            myStreamWriter.Write(areaAnalizar.Text)
            myStreamWriter.Flush()
        Catch ex As Exception

        End Try
    End Sub

    Public Sub metodo_salir()
        Close()
    End Sub

    Private Sub menuabrir_Click(sender As Object, e As EventArgs) Handles menuabrir.Click
        metodo_abrir()
    End Sub

    Private Sub menuguardar_Click(sender As Object, e As EventArgs) Handles menuguardar.Click
        metodo_guardar()
    End Sub

    Private Sub menuguardarcomo_Click(sender As Object, e As EventArgs) Handles menuguardarcomo.Click
        metodo_guardarcomo()
    End Sub

    Private Sub menusalir_Click(sender As Object, e As EventArgs) Handles menusalir.Click
        metodo_salir()
    End Sub


    Private Sub ManualDeLaAplicacionToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ManualDeLaAplicacionToolStripMenuItem.Click
        Dim loPSI As New ProcessStartInfo
        Dim loPSI2 As New ProcessStartInfo
        Dim proceso As New Process
        Dim proceso2 As New Process
        loPSI.FileName = "manual_usuario.pdf"
        loPSI2.FileName = "manual_tecnico.pdf"
        Try
            proceso = Process.Start(loPSI)
            proceso2 = Process.Start(loPSI2)
        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try
    End Sub

    Private Sub AcercaDeToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AcercaDeToolStripMenuItem.Click
        Dim info As String = "Que es Analilisis lexico?" + vbLf + "El analizador léxico es la primera fase de un compilador." + vbLf + "Su principal función consiste en leer los caracteres de entrada y elaborar como salida una secuencia de componentes léxicos que utiliza el analizador sintáctico para hacer el análisis." + vbLf + vbLf + "Que es Analilisis Sintactico?" + vbLf + "El analizador Sintactivo es la segunda fase de un compilador." + vbLf + "En esta fase se analiza la estructura de las expresiones en base a gramáticas, Aquí ya se puede determinar si una estructura esta mal formada."
        Dim info2 As String = "Version de app: 3.0" + vbLf + "Desarrollado en Visual Basic, utilizando Visual Estudio 2013"
        MessageBox.Show("Realizado por: Christopher Alexander Acajabon Gudiel" + vbLf + vbLf + "Carnet: 201404278" + vbLf + vbLf + info2 + vbLf + vbLf + info)
    End Sub

    Private Sub DdToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DdToolStripMenuItem.Click

        Dim loPSI As New ProcessStartInfo
        Dim proceso As New Process
        loPSI.FileName = "C:\Users\GUDIEL\Desktop\Reportes\Reporte_Tokens.html"

        Try
            proceso = Process.Start(loPSI)

        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try
    End Sub

    Private Sub ErroresToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ErroresToolStripMenuItem.Click
        Dim loPSI As New ProcessStartInfo
        Dim proceso As New Process
        loPSI.FileName = "C:\Users\GUDIEL\Desktop\Reportes\Reporte_Errores.html"

        Try
            proceso = Process.Start(loPSI)

        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try
    End Sub

    Private Sub GrafoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GrafoToolStripMenuItem.Click

        Dim loPSI As New ProcessStartInfo
        Dim proceso As New Process
        loPSI.FileName = "grafo.jpg"

        Try
            proceso = Process.Start(loPSI)

        Catch Exp As Exception
            MessageBox.Show(Exp.Message, "XXXX", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try
    End Sub

    Public Sub contarLlaves()


        For Each row As DataGridViewRow In dgvbuena.Rows
            If row.Cells(1).Value = "{" Then
                contadorLlaves += 1
            End If
        Next

        For Each row2 As DataGridViewRow In dgvbuena.Rows
            If row2.Cells(1).Value = "}" Then
                contadorLlaves2 += 1
            End If
        Next

        If contadorLlaves > contadorLlaves2 Then
            Console.WriteLine("Falta de }")
            contador_error_2 = contador_error_2 + 1
            dgverror2.Rows.Add(contador_error_2, "Signo }", " falta llave de cierre: }", "No identificado", "No identificado")
        ElseIf contadorLlaves < contadorLlaves2 Then
            Console.WriteLine("Falta de {")
            contador_error_2 = contador_error_2 + 1
            dgverror2.Rows.Add(contador_error_2, "Signo {", "falta llave de apertura {", "No identificado", "No identificado")
        End If


    End Sub

    Private Sub AnalizarLexicoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AnalizarLexicoToolStripMenuItem.Click
        iniciarAnalisis()
    End Sub

    Private Sub bttprueba_Click_1(sender As Object, e As EventArgs) Handles bttprueba.Click

    End Sub


    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Timer1.Interval = 10
        Timer1.Start()
 
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        PictureBox1.Refresh()
    End Sub

    Private Sub PictureBox1_Paint(sender As Object, e As PaintEventArgs) Handles PictureBox1.Paint
        caracterlinea = 0
        Dim altura As Integer = areaAnalizar.GetPositionFromCharIndex(0).Y
        If areaAnalizar.Lines.Length > 0 Then
            For i = 0 To areaAnalizar.Lines.Length - 1
                e.Graphics.DrawString(i + 1, areaAnalizar.Font, Brushes.Yellow, PictureBox1.Width - (e.Graphics.MeasureString(i + 1, areaAnalizar.Font).Width + 10), altura)
                caracterlinea += areaAnalizar.Lines(i).Length + 1
                altura = areaAnalizar.GetPositionFromCharIndex(caracterlinea).Y

            Next


        Else
            e.Graphics.DrawString(1, areaAnalizar.Font, Brushes.Black, PictureBox1.Width - (e.Graphics.MeasureString(1, areaAnalizar.Font).Width + 10), altura)
        End If
    End Sub

    Private Sub FuenteToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FuenteToolStripMenuItem.Click
        fdFuentes.ShowDialog()
        areaAnalizar.Font = fdFuentes.Font
    End Sub

    Private Sub ColorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ColorToolStripMenuItem.Click
        cdColores.ShowDialog()
        areaAnalizar.ForeColor = cdColores.Color
    End Sub

    'para personalizar botones

    'cuando esta encima el puntero
    Private Sub bttanalizar_MouseMove(sender As Object, e As MouseEventArgs) Handles bttanalizar.MouseMove
        Me.bttanalizar.ForeColor = Color.Yellow
    End Sub

    'cuando no esta encima el puntero
    Private Sub bttanalizar_MouseLeave(sender As Object, e As EventArgs) Handles bttanalizar.MouseLeave
        Me.bttanalizar.ForeColor = Color.White
    End Sub

    'cuando esta encima el puntero
    Private Sub bttlimpiar_MouseMove(sender As Object, e As MouseEventArgs) Handles bttlimpiar.MouseMove
        Me.bttlimpiar.ForeColor = Color.Yellow
    End Sub

    'cuando no esta encima el puntero
    Private Sub bttlimpiar_MouseLeave(sender As Object, e As EventArgs) Handles bttlimpiar.MouseLeave
        Me.bttlimpiar.ForeColor = Color.White
    End Sub

    'REPORTES
    Private Sub ReporteToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles ReporteToolStripMenuItem.MouseMove
        Me.ReporteToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub ReporteToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles ReporteToolStripMenuItem.MouseLeave
        Me.ReporteToolStripMenuItem.ForeColor = Color.White
    End Sub

    Private Sub DdToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles DdToolStripMenuItem.MouseMove
        Me.ReporteToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub ErroresToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles ErroresToolStripMenuItem.MouseMove
        Me.ReporteToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub GrafoToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles GrafoToolStripMenuItem.MouseMove
        Me.ReporteToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub DdToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles DdToolStripMenuItem.MouseLeave
        Me.ReporteToolStripMenuItem.ForeColor = Color.White
    End Sub

    Private Sub ErroresToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles ErroresToolStripMenuItem.MouseLeave
        Me.ReporteToolStripMenuItem.ForeColor = Color.White
    End Sub

    Private Sub GrafoToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles GrafoToolStripMenuItem.MouseLeave
        Me.ReporteToolStripMenuItem.ForeColor = Color.White
    End Sub

    'ARCHIVO
    Private Sub ArchivoToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles ArchivoToolStripMenuItem.MouseMove
        Me.ArchivoToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub ArchivoToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles ArchivoToolStripMenuItem.MouseLeave
        Me.ArchivoToolStripMenuItem.ForeColor = Color.White
    End Sub

    Private Sub menuabrir_MouseMove(sender As Object, e As MouseEventArgs) Handles menuabrir.MouseMove
        Me.ArchivoToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub menuabrir_MouseLeave(sender As Object, e As EventArgs) Handles menuabrir.MouseLeave
        Me.ArchivoToolStripMenuItem.ForeColor = Color.White
    End Sub

    Private Sub menuguardar_MouseMove(sender As Object, e As MouseEventArgs) Handles menuguardar.MouseMove
        Me.ArchivoToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub menuguardar_MouseLeave(sender As Object, e As EventArgs) Handles menuguardar.MouseLeave
        Me.ArchivoToolStripMenuItem.ForeColor = Color.White
    End Sub

    Private Sub menuguardarcomo_MouseMove(sender As Object, e As MouseEventArgs) Handles menuguardarcomo.MouseMove
        Me.ArchivoToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub menuguardarcomo_MouseLeave(sender As Object, e As EventArgs) Handles menuguardarcomo.MouseLeave
        Me.ArchivoToolStripMenuItem.ForeColor = Color.White
    End Sub

    Private Sub menusalir_MouseMove(sender As Object, e As MouseEventArgs) Handles menusalir.MouseMove
        Me.ArchivoToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub menusalir_MouseLeave(sender As Object, e As EventArgs) Handles menusalir.MouseLeave
        Me.ArchivoToolStripMenuItem.ForeColor = Color.White
    End Sub

    'ANALIZAR
    Private Sub AnalizarToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles AnalizarToolStripMenuItem.MouseMove
        Me.AnalizarToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub AnalizarToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles AnalizarToolStripMenuItem.MouseLeave
        Me.AnalizarToolStripMenuItem.ForeColor = Color.White
    End Sub

    Private Sub AnalizarLexicoToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles AnalizarLexicoToolStripMenuItem.MouseMove
        Me.AnalizarToolStripMenuItem.ForeColor = Color.Black
    End Sub

    Private Sub AnalizarLexicoToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles AnalizarLexicoToolStripMenuItem.MouseLeave
        Me.AnalizarToolStripMenuItem.ForeColor = Color.White
    End Sub

    'AYUDA
    Private Sub AyudaToolStripMenuItem1_MouseMove(sender As Object, e As MouseEventArgs) Handles AyudaToolStripMenuItem1.MouseMove
        Me.AyudaToolStripMenuItem1.ForeColor = Color.Black
    End Sub

    Private Sub AyudaToolStripMenuItem1_MouseLeave(sender As Object, e As EventArgs) Handles AyudaToolStripMenuItem1.MouseLeave
        Me.AyudaToolStripMenuItem1.ForeColor = Color.White
    End Sub

    Private Sub ManualDeLaAplicacionToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles ManualDeLaAplicacionToolStripMenuItem.MouseMove
        Me.AyudaToolStripMenuItem1.ForeColor = Color.Black
    End Sub

    Private Sub ManualDeLaAplicacionToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles ManualDeLaAplicacionToolStripMenuItem.MouseLeave
        Me.AyudaToolStripMenuItem1.ForeColor = Color.White
    End Sub

    Private Sub AcercaDeToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles AcercaDeToolStripMenuItem.MouseMove
        Me.AyudaToolStripMenuItem1.ForeColor = Color.Black
    End Sub

    Private Sub AcercaDeToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles AcercaDeToolStripMenuItem.MouseLeave
        Me.AyudaToolStripMenuItem1.ForeColor = Color.White
    End Sub

    'EDICION
    Private Sub EdicionToolStripMenuItem1_MouseMove(sender As Object, e As MouseEventArgs) Handles EdicionToolStripMenuItem1.MouseMove
        Me.EdicionToolStripMenuItem1.ForeColor = Color.Black
    End Sub

    Private Sub EdicionToolStripMenuItem1_MouseLeave(sender As Object, e As EventArgs) Handles EdicionToolStripMenuItem1.MouseLeave
        Me.EdicionToolStripMenuItem1.ForeColor = Color.White
    End Sub

    Private Sub FuenteToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles FuenteToolStripMenuItem.MouseMove
        Me.EdicionToolStripMenuItem1.ForeColor = Color.Black
    End Sub

    Private Sub FuenteToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles FuenteToolStripMenuItem.MouseLeave
        Me.EdicionToolStripMenuItem1.ForeColor = Color.White
    End Sub

    Private Sub ColorToolStripMenuItem_MouseMove(sender As Object, e As MouseEventArgs) Handles ColorToolStripMenuItem.MouseMove
        Me.EdicionToolStripMenuItem1.ForeColor = Color.Black
    End Sub

    Private Sub ColorToolStripMenuItem_MouseLeave(sender As Object, e As EventArgs) Handles ColorToolStripMenuItem.MouseLeave
        Me.EdicionToolStripMenuItem1.ForeColor = Color.White
    End Sub

End Class
