Imports System.IO
Imports System.Text
Module Module1
    'Глобальные данные
    Dim k As Byte = 0
    Dim speed As Single = 0
    Dim result As Boolean
    Dim playerHp As Integer = 140
    Dim playerSoda As Integer = 70
    Dim playerCoins As Integer = 25
    Dim playerStamina As Integer = 14
    Dim playerBag As Integer = 0
    ReadOnly playerIdols As Integer = 0
    ReadOnly playerVip As Boolean = 0
    Dim maxPlayerXp As Integer = 140
    ReadOnly maxPlayerBag As Integer = 6
    ReadOnly userName As String = "PIP"
    ReadOnly maxPlayerSoda As Integer = 70
    Dim playerInventory(5) As Boolean
    Dim playerInventoryNames(5) As String
    Dim doneLevelsFromCharpet1(3) As Boolean
    Dim doneLevelsFromCharpet2(3) As Boolean
    Dim doneLevelsFromCharpet3(3) As Boolean
    Dim doneChapters(2) As Boolean
    Dim done_first_season_tasks(11) As Boolean
    Dim first_win_battle As Boolean
    Dim defeat_3_crearures(2, 4) As Byte
    Dim is_here_book_of_the_damned As Boolean
    Dim card_and_standart_games(1, 4) As Boolean
    Dim save_param() As String =
            {
                "player_coins = ",
                "inventory = ",
                "done_chapters = ",
                "done_levels_from_chapter_1 = ",
                "done_levels_from_chapter_2 = ",
                "done_levels_from_chapter_3 = ",
                "done_tasks = ",
                "f_t_d_t = ",
                "kingdom_of_snails = "
            }
    Dim first_time_done_tasks(11) As Boolean
    Dim past_games(1) As Boolean
    Dim kingdom_of_snails As Boolean
    Sub Check_tasks_for_complete()

        Dim special_num As Integer
        Dim str, i As String
        i = 0

        If done_first_season_tasks(10) Then
            If Not first_time_done_tasks(10) Then
                playerCoins += 3500
                first_time_done_tasks(10) = True
            End If
        End If

        If done_first_season_tasks(9) Then
            If Not first_time_done_tasks(9) Then
                playerCoins += 3500
                first_time_done_tasks(9) = True
            End If
        End If

        If done_first_season_tasks(11) Then
            If Not first_time_done_tasks(11) Then
                playerCoins += 7000
                first_time_done_tasks(11) = True
            End If
        End If

        If done_first_season_tasks(7) Then
            If Not first_time_done_tasks(7) Then
                playerCoins += 750
                first_time_done_tasks(7) = True
            End If
        End If

        If done_first_season_tasks(6) Then
            If Not first_time_done_tasks(6) Then
                playerCoins += 750
                first_time_done_tasks(6) = True
            End If
        End If

        If done_first_season_tasks(5) Then
            If Not first_time_done_tasks(5) Then
                playerCoins += 750
                first_time_done_tasks(5) = True
            End If
        End If

        If doneChapters(0) Then
            If Not first_time_done_tasks(3) Then
                playerCoins += 1300
                first_time_done_tasks(3) = True
            End If
        End If

        If playerCoins >= 2000 Then
            done_first_season_tasks(4) = True
            If Not first_time_done_tasks(4) Then
                playerCoins += 1000
                first_time_done_tasks(4) = True
            End If
        End If

        If playerCoins >= 200 Then
            done_first_season_tasks(1) = True
            If Not first_time_done_tasks(1) Then
                playerCoins += 200
                first_time_done_tasks(1) = True
            End If
        End If

        If playerBag = 6 Then
            done_first_season_tasks(2) = True
            If Not first_time_done_tasks(2) Then
                playerCoins += 200
                first_time_done_tasks(2) = True
            End If
        End If

        If is_here_book_of_the_damned Then
            done_first_season_tasks(8) = True
        End If

        For j = 0 To UBound(playerInventoryNames)
            If playerInventoryNames(j) = "NULL" Then
                playerInventory(j) = False
            End If
        Next

        For j = 0 To UBound(playerInventory)
            If Not playerInventory(j) Then
                playerInventoryNames(j) = "NULL"
            End If
        Next

        For j = 0 To UBound(playerInventoryNames)
            If playerInventoryNames(j) = "BOOK OF THE DAMNED" Then
                done_first_season_tasks(8) = True
                If Not first_time_done_tasks(8) Then
                    playerCoins += 1100
                    first_time_done_tasks(8) = True
                End If
            End If
        Next

        If first_win_battle Then
            done_first_season_tasks(0) = True
            If Not first_time_done_tasks(0) Then
                playerCoins += 150
                first_time_done_tasks(0) = True
            End If
        End If

        FileOpen(1, "radio_room/save.txt", OpenMode.Output)

        Do
            str = save_param(i)

            Select Case i
                Case 0
                    str &= playerCoins
                Case 1
                    For j = 0 To UBound(playerInventory)
                        str &= playerInventoryNames(j)
                        If j = UBound(playerInventory) Then
                        Else
                            str &= ","
                        End If
                    Next
                Case 2
                    For j = 0 To UBound(doneChapters)
                        If doneChapters(j) Then
                            special_num = j + 1
                            str &= special_num.ToString
                        Else
                            str &= "0"
                        End If
                        If Not j = UBound(doneChapters) Then
                            str &= ","
                        End If
                    Next
                Case 3
                    For j = 0 To UBound(doneLevelsFromCharpet1)
                        If doneLevelsFromCharpet1(j) Then
                            special_num = j + 1
                            str &= special_num.ToString
                        Else
                            str &= "0"
                        End If
                        If Not j = UBound(doneLevelsFromCharpet1) Then
                            str &= ","
                        End If
                    Next
                Case 4
                    For j = 0 To UBound(doneLevelsFromCharpet2)
                        If doneLevelsFromCharpet2(j) Then
                            special_num = j + 1
                            str &= special_num.ToString
                        Else
                            str &= "0"
                        End If
                        If Not j = UBound(doneLevelsFromCharpet2) Then
                            str &= ","
                        End If
                    Next
                Case 5
                    For j = 0 To UBound(doneLevelsFromCharpet3)
                        If doneLevelsFromCharpet3(j) Then
                            special_num = j + 1
                            str &= special_num.ToString
                        Else
                            str &= "0"
                        End If
                        If Not j = UBound(doneLevelsFromCharpet3) Then
                            str &= ","
                        End If
                    Next
                Case 6
                    For j = 0 To UBound(done_first_season_tasks)
                        If done_first_season_tasks(j) Then
                            special_num = j + 1
                            str &= special_num.ToString
                        Else
                            str &= "0"
                        End If
                        If Not j = UBound(done_first_season_tasks) Then
                            str &= ","
                        End If
                    Next
                Case 7
                    For j = 0 To UBound(first_time_done_tasks)
                        If first_time_done_tasks(j) Then
                            str &= "1"
                        Else
                            str &= "0"
                        End If
                        If Not j = UBound(first_time_done_tasks) Then
                            str &= ","
                        End If
                    Next
                Case 8
                    If kingdom_of_snails Then
                        str &= "True"
                    Else
                        str &= "False"
                    End If
            End Select

            PrintLine(1, str)

            i += 1
        Loop Until i = 9
        FileClose(1)
    End Sub
    Function RandomNum(max As Integer, min As Integer) As Integer
        Dim rndNum As Integer
        Randomize()
        rndNum = Rnd() * (max - min) + min

        Return rndNum
    End Function
    'Проверка строки, введенной пользователем |В определенном отрезке чисел x - y|
    Function CheckUserAns(usAns As String, firstNum As Integer, secondNum As Integer) As Boolean
        Dim num As Single
        Dim res As Boolean

        If IsNumeric(usAns) Then
            If (usAns > 2000000000) Or (usAns < -2000000000) Then
                res = False
                Return res
            End If
            num = usAns
            If (num Mod 1) = 0 Then
                If (num >= firstNum) And (num <= secondNum) Then
                    res = True
                End If
            End If
        End If

        Return res
    End Function
    'Вывод состояния загрузки окна
    Sub Overload(i As Integer, j As Integer)
        Dim str As String = "||||||||||||||"

        Console.SetCursorPosition(i, j)
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.WriteLine("==============")
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.White
        Console.SetCursorPosition(i, j + 1)

        For i = 0 To str.Length - 1
            Console.Write(str(i))
            Pause(0.33)
        Next

        Console.ResetColor()
    End Sub
    'Пауза
    Sub Pause(d As Single)
        Dim t As Single
        t = Timer
        Do Until Timer - t > d
        Loop
    End Sub
    'Вывод строки посимвольно
    Sub LineOutPutCharacterByCharacter(str As String, i1 As Integer, y1 As Integer, speed As Single, ConsoleColor As ConsoleColor)
        Dim symb As Char

        Console.SetCursorPosition(i1, y1)
        Console.ForegroundColor = ConsoleColor
        For i = 0 To str.Length - 1
            symb = str(i)
            Console.Write(symb)
            Pause(speed)
        Next
        Console.ResetColor()
    End Sub
    'Вывод изображения построчно
    Sub ImageOutputLineByLine(m() As String, speed As Single, indent As Integer)
        For i = 0 To UBound(m)
            If indent <> 0 Then
                Console.Write(StrDup(indent, " "))
            End If
            Console.WriteLine(m(i))
            Pause(speed)
        Next
    End Sub
    'Вывод изображения сбоку
    Sub DisplayingPictureFromSide(m() As String, i1 As Integer, j1 As Integer, speed As Single)
        For i = 0 To m(0).Length
            For j = 0 To UBound(m)
                Console.SetCursorPosition(i + i1, j + j1)
                Console.Write(Mid(m(j), i + 1, j + 1))
                Pause(speed)
            Next
        Next
    End Sub
    Function GameModes() As String
        Dim gameModesList() As String =
            {
                "STANDART",
                "CARD GAME"
            }

        Dim rndNum As Integer

        If past_games(0) = True And past_games(1) = True Then
            Return gameModesList(1)
        ElseIf past_games(0) = False And past_games(1) = False Then
            Return gameModesList(0)
        Else
            Randomize()
            rndNum = Rnd() * (1 - 0) + 0

            If rndNum = 1 Then
                Return gameModesList(1)
            Else
                Return gameModesList(0)
            End If
        End If
    End Function
    Function DisplayingDialogBox(dialogBoxName As String) As String()
        Dim simpleDialogWindow() As String = {
            " ╬══════════════════════════════════════════════════════╬ ",
            "╬║                                                      ║╬",
            "║║                                                      ║║",
            "║║                                                      ║║",
            "║║                                                      ║║",
            "║║                                                      ║║",
            "╬║                                                      ║╬",
            " ╬══════════════════════════════════════════════════════╬ "
                                   }

        Dim somethgWentWrong() As String = {
            "  ╬═════════════════════════════════╬  ",
            "║╬║             ╔╗╔╗╔╗              ║╬║",
            "╬║║        ╔═╦╦═╣╚╣╚╬╬═╦╦═╗         ║║╬",
            "║║║        ║║║║╬║╔╣║║║║║║╬║         ║║║",
            "║║║        ╚╩═╩═╩═╩╩╩╩╩═╬╗║         ║║║",
            "╬║║                     ╚═╝         ║║╬",
            "║╬║                                 ║╬║",
            "  ╬═════════════════════════════════╬  "
                                           }

        Select Case dialogBoxName
            Case "simpleDialogWindow"
                Return simpleDialogWindow
        End Select

        Return somethgWentWrong
    End Function
    Function IssuingAnIcon(iconName As String) As String()
        Dim iconDoorEssence() As String = {
            "╬═══════════╬",
            "║│││█████│││║",
            "║││█│││││█││║",
            "║││╬││││█│││║",
            "║││││││█││││║",
            "║│││││││││││║",
            "║│││││█│││││║",
            "╬═══════════╬"
                                          }
        Dim somethgWentWrong() As String = {
            "╬═══════════╬",
            "║█││║││║│││█║",
            "║│█│║││║││█│║",
            "║││█║╬╬║█│││║",
            "║││█║╬╬║█│││║",
            "║│█│║││║│█││║",
            "║█││║││║││█│║",
            "╬═══════════╬"
                                           }

        Dim pip() As String =
            {
                "╬═══════════╬",
                "║│││╔═══╗│││║",
                "║│││║╔═╗║│││║",
                "║│││║╚═╝║│││║",
                "║│││║╔══╝│││║",
                "║│││║║││││││║",
                "║│││╚╝││││││║",
                "╬═══════════╬"
            }

        Dim lars() As String =
            {
                "╬═══════════╬",
                "║││││╔╗│││││║",
                "║││││║║│││││║",
                "║││││║║│││││║",
                "║││││║║│││││║",
                "║││││║╚═╗│││║",
                "║││││╚══╝│││║",
                "╬═══════════╬"
            }

        Dim knight() As String =
            {
                "╬═══════════╬",
                "║│││╔╗╔═╗│││║",
                "║│││║║║╔╝│││║",
                "║│││║╚╝╝││││║",
                "║│││║╔╗║││││║",
                "║│││║║║╚╗│││║",
                "║│││╚╝╚═╝│││║",
                "╬═══════════╬"
            }

        Dim magician() As String =
            {
                "╬═══════════╬",
                "║│││╔═╗╔═╗││║",
                "║│││║║╚╝║║││║",
                "║│││║╔╗╔╗║││║",
                "║│││║║║║║║││║",
                "║│││║║║║║║││║",
                "║│││╚╝╚╝╚╝││║",
                "╬═══════════╬"
            }

        Dim mrSlime() As String =
            {
                "╬═══════════╬",
                "║│││╔═══╗│││║",
                "║│││║╔═╗║│││║",
                "║│││║╚══╗│││║",
                "║│││╚══╗║│││║",
                "║│││║╚═╝║│││║",
                "║│││╚═══╝│││║",
                "╬═══════════╬"
            }

        Dim magicChest() As String =
            {
                "╬═══════════╬",
                "║│││╔═══╗│││║",
                "║│││║╔═╗║│││║",
                "║│││║║│╚╝│││║",
                "║│││║║│╔╗│││║",
                "║│││║╚═╝║│││║",
                "║│││╚═══╝│││║",
                "╬═══════════╬"
            }

        Select Case iconName
            Case "iconDoorEssence"
                Return iconDoorEssence
            Case "iconlars"
                Return lars
            Case "mrpip"
                Return pip
            Case "knight"
                Return knight
            Case "mrSlime"
                Return mrSlime
            Case "magician"
                Return magician
            Case "magicchest"
                Return magicChest
        End Select

        Return somethgWentWrong
    End Function
    Function BetsOfBots() As Integer
        Dim result As Integer

        Dim pass As Boolean

        Randomize()

        Do
            If playerCoins >= 110 Then
                result = 100 + CInt(((playerCoins) - 100 + 1) * Rnd())
            Else
                result = 100 + CInt(((1000) - 100 + 1) * Rnd())
            End If
            If result Mod 5 = 0 Then
                pass = True
            End If
        Loop Until pass

        Return result
    End Function
    'Функция - решение бота - прикупить/остановиться
    Function TakeСardOrStop(denomination(,) As Integer, player As Integer, gameBets() As Integer) As Boolean
        Dim sum, numberOfTheDealer As Integer
        numberOfTheDealer = UBound(denomination, 2)
        Dim result As Boolean

        For i = 0 To UBound(denomination, 1) - 1
            sum += denomination(i, player)
        Next

        If sum >= 17 And sum <= 19 Then
            If sum = 17 Then
                If denomination(1, numberOfTheDealer) >= 7 Then
                    result = True
                Else
                    result = False
                End If
            Else
                result = False
            End If
        End If


        If sum <= 16 Then
            result = True
        End If

        If sum = 20 Or sum = 21 Or sum = 19 Or sum = 18 Then
            result = False
        End If

        Return result
    End Function
    Function DenominationOfCard(num As Integer) As Integer
        Dim res As Integer
        Select Case num
            Case 0
                res = 2
            Case 1
                res = 3
            Case 2
                res = 4
            Case 3
                res = 5
            Case 4
                res = 6
            Case 5
                res = 7
            Case 6
                res = 8
            Case 7
                res = 9
            Case 8
                res = 10
            Case 9 To 11
                res = 10
            Case 12
                res = 11
        End Select

        Return res
    End Function

    'Карты список
    Function PlayingCardsList(num As Integer) As String()
        Dim card2() As String = {
            "┼────────────┼",
            "│░▄▄▄░░╬──╬░░│",
            "│░▄▄█░░│┼┼│░░│",
            "│░█▄▄░░╬──╬░░│",
            "│░░░░░░░░░░░░│",
            "│░░░░░░░░░░░░│",
            "│░╬──╬░░▄▄▄░░│",
            "│░│┼┼│░░▄▄█░░│",
            "│░╬──╬░░█▄▄░░│",
            "│░░░░░░░░░░░░│",
            "┼────────────┼"
                                 }
        Dim card3() As String = {
            "┼────────────┼",
            "│░▄▄▄░░╬──╬░░│",
            "│░▄▄█░░│┼┼│░░│",
            "│░▄▄█░░╬──╬░░│",
            "│░░░░░░░░░░░░│",
            "│░░░░░░░░░░░░│",
            "│░╬──╬░░░▄▄▄░│",
            "│░│┼┼│░░░▄▄█░│",
            "│░╬──╬░░░▄▄█░│",
            "│░░░░░░░░░░░░│",
            "┼────────────┼"
                                 }
        Dim card4() As String = {
            "┼────────────┼",
            "│░▄░▄░░╬──╬░░│",
            "│░█▄█░░│┼┼│░░│",
            "│░░░█░░╬──╬░░│",
            "│░░░░░░░░░░░░│",
            "│░░░░░░░░░░░░│",
            "│░╬──╬░░▄░▄░░│",
            "│░│┼┼│░░█▄█░░│",
            "│░╬──╬░░░░█░░│",
            "│░░░░░░░░░░░░│",
            "┼────────────┼"
                                 }
        Dim card5() As String = {
            "┼────────────┼",
            "│░▄▄▄░░╬──╬░░│",
            "│░█▄▄░░│┼┼│░░│",
            "│░▄▄█░░╬──╬░░│",
            "│░░░░░░░░░░░░│",
            "│░░░░░░░░░░░░│",
            "│░╬──╬░░▄▄▄░░│",
            "│░│┼┼│░░█▄▄░░│",
            "│░╬──╬░░▄▄█░░│",
            "│░░░░░░░░░░░░│",
            "┼────────────┼"
                                 }
        Dim card6() As String = {
            "┼────────────┼",
            "│░▄▄▄░░╬──╬░░│",
            "│░█▄▄░░│┼┼│░░│",
            "│░█▄█░░╬──╬░░│",
            "│░░░░░░░░░░░░│",
            "│░░░░░░░░░░░░│",
            "│░╬──╬░░▄▄▄░░│",
            "│░│┼┼│░░█▄▄░░│",
            "│░╬──╬░░█▄█░░│",
            "│░░░░░░░░░░░░│",
            "┼────────────┼"
                                 }
        Dim card7() As String = {
            "┼────────────┼",
            "│░▄▄▄░░╬──╬░░│",
            "│░░░█░░│┼┼│░░│",
            "│░░░█░░╬──╬░░│",
            "│░░░░░░░░░░░░│",
            "│░░░░░░░░░░░░│",
            "│░╬──╬░░▄▄▄░░│",
            "│░│┼┼│░░░░█░░│",
            "│░╬──╬░░░░█░░│",
            "│░░░░░░░░░░░░│",
            "┼────────────┼"
                                 }
        Dim card8() As String = {
            "┼────────────┼",
            "│░▄▄▄░░╬──╬░░│",
            "│░█▄█░░│┼┼│░░│",
            "│░█▄█░░╬──╬░░│",
            "│░░░░░░░░░░░░│",
            "│░░░░░░░░░░░░│",
            "│░╬──╬░░▄▄▄░░│",
            "│░│┼┼│░░█▄█░░│",
            "│░╬──╬░░█▄█░░│",
            "│░░░░░░░░░░░░│",
            "┼────────────┼"
                                 }
        Dim card9() As String = {
            "┼────────────┼",
            "│░▄▄▄░░╬──╬░░│",
            "│░█▄█░░│┼┼│░░│",
            "│░▄▄█░░╬──╬░░│",
            "│░░░░░░░░░░░░│",
            "│░░░░░░░░░░░░│",
            "│░╬──╬░░▄▄▄░░│",
            "│░│┼┼│░░█▄█░░│",
            "│░╬──╬░░▄▄█░░│",
            "│░░░░░░░░░░░░│",
            "┼────────────┼"
                                 }
        Dim card10() As String = {
            "┼────────────┼",
            "│░▄░▄▄▄░╬──╬░│",
            "│░█░█ █░│┼┼│░│",
            "│░█░█▄█░╬──╬░│",
            "│░░░░░░░░░░░░│",
            "│░░░░░░░░░░░░│",
            "│░╬──╬░▄░▄▄▄░│",
            "│░│┼┼│░█░█░█░│",
            "│░╬──╬░█░█▄█░│",
            "│░░░░░░░░░░░░│",
            "┼────────────┼"
                                  }
        Dim cardJack() As String = {
            "┼────────────┼",
            "│░░░░░░░██╗░░│",
            "│░░░░░░░██║░░│",
            "│░░░░░░░██║░░│",
            "│░░██╗░░██║░░│",
            "│░░╚█████╔╝░░│",
            "│░░░╚════╝░░░│",
            "│░░╬═════╬░░░│",
            "│░░║┼┼╬┼┼║░░░│",
            "│░░───────░░░│",
            "┼────────────┼"
                                    }
        Dim cardQueen() As String = {
            "┼────────────┼",
            "│░░░██████╗░░│",
            "│░░██╔═══██╗░│",
            "│░░██║██╗██║░│",
            "│░░╚██████╔╝░│",
            "│░░░╚═██╔═╝░░│",
            "│░░░░░╚═╝░░░░│",
            "│░░╬═════╬░░░│",
            "│░░║┼┼╬┼┼║░░░│",
            "│░░───────░░░│",
            "┼────────────┼"
                                     }
        Dim cardKing() As String = {
            "┼────────────┼",
            "│░░██╗░░██╗░░│",
            "│░░██║░██╔╝░░│",
            "│░░█████═╝░░░│",
            "│░░██╔═██╗░░░│",
            "│░░██║░╚██╗░░│",
            "│░░╚═╝░░╚═╝░░│",
            "│░░╬═════╬░░░│",
            "│░░║┼┼╬┼┼║░░░│",
            "│░░───────░░░│",
            "┼────────────┼"
                                    }
        Dim cardAce() As String = {
            "┼────────────┼",
            "│░░░█████╗░░░│",
            "│░░██╔══██╗░░│",
            "│░░███████║░░│",
            "│░░██╔══██║░░│",
            "│░░██║░░██║░░│",
            "│░░╚═╝░░╚═╝░░│",
            "│░░╬═════╬░░░│",
            "│░░║┼┼╬┼┼║░░░│",
            "│░░───────░░░│",
            "┼────────────┼"
                                   }
        Dim сardСover() As String = {
            "┼────────────┼",
            "│╬────┼┼────╬│",
            "││▒▒▒▒││▒▒▒▒││",
            "││▒▒▒▒││▒▒▒▒││",
            "││▒▒▒▒││▒▒▒▒││",
            "│┼────╬╬────┼│",
            "││▒▒▒▒││▒▒▒▒││",
            "││▒▒▒▒││▒▒▒▒││",
            "││▒▒▒▒││▒▒▒▒││",
            "│╬────┼┼────╬│",
            "┼────────────┼"
                                    }
        Select Case num
            Case 0
                Return card2
            Case 1
                Return card3
            Case 2
                Return card4
            Case 3
                Return card5
            Case 4
                Return card6
            Case 5
                Return card7
            Case 6
                Return card8
            Case 7
                Return card9
            Case 8
                Return card10
            Case 9
                Return cardJack
            Case 10
                Return cardQueen
            Case 11
                Return cardKing
            Case 12
                Return cardAce
            Case 13
                Return сardСover
        End Select

        Return сardСover
    End Function
    'Просмотр карт
    Sub ShowCard(amountOfCards As Integer, i1 As Integer, j1 As Integer, ByRef playersCards(,) As Integer, player As Integer, spNum As Integer, denomination(,) As Integer, ByRef sumOfCardsOfPlayers() As Integer)
        Dim num, sum As Integer
        Dim m() As String

        'Подсчет суммы карт у игрока (player)

        If player <> 3 Then
            For i = 0 To UBound(denomination, 1)
                sum += denomination(i, player)
            Next
            sumOfCardsOfPlayers(player) = sum
        End If

        If spNum = 0 Then
            For i = 0 To amountOfCards - 1
                num = playersCards(i, player)
                m = PlayingCardsList(num)
                For j = 0 To UBound(m)
                    Console.SetCursorPosition(i1, j1 + j)
                    Console.WriteLine(m(j))
                Next
                i1 += 13
                Console.SetCursorPosition(i1 - 2, j1)
                Console.ForegroundColor = ConsoleColor.Red
                Console.WriteLine(denomination(i, player))
                Console.ResetColor()
            Next
        Else
            num = playersCards(spNum, player)
            m = PlayingCardsList(num)
            For i = 0 To UBound(m)
                Console.SetCursorPosition(i1, j1 + i)
                Console.WriteLine(m(i))
            Next
            Console.SetCursorPosition(i1 + 11, j1)
            Console.ForegroundColor = ConsoleColor.Red
            Console.WriteLine(denomination(spNum, player))
            Console.ResetColor()
        End If
    End Sub
    'Выдача карт
    Sub TakeCard(amountOfCards As Integer, ByRef playersCards(,) As Integer, player As Integer, specPlace As Integer, denomination(,) As Integer)
        Dim rndNum, amountOfDenomination As Integer
        Dim m() As String

        For i = 0 To amountOfCards - 1
            rndNum = randomNumForCards()
            m = PlayingCardsList(rndNum)
            amountOfDenomination = DenominationOfCard(rndNum)
            If specPlace = 0 Then
                playersCards(i, player) = rndNum
                denomination(i, player) = amountOfDenomination
            Else
                playersCards(specPlace, player) = rndNum
                denomination(specPlace, player) = amountOfDenomination
            End If
        Next

    End Sub
    Function RandomNumForCards() As Integer
        Dim rndNum As Integer
        Randomize()
        rndNum = 0 + CInt((11 - 0 + 1) * Rnd())

        Return rndNum
    End Function
    Function EnemyProtection() As Integer
        Dim rndNum As Integer

        Randomize()
        rndNum = Rnd() * (20 - 1) + 1
        Select Case rndNum
            Case 1 To 5
                Return 0
            Case 6 To 9
                Return 1
            Case 10 To 12
                Return 2
            Case 13 To 16
                Return 3
            Case 17 To 18
                Return 4
            Case 19 To 20
                Return 5
        End Select

        Return 5
    End Function
    Function HitSelection(enemyHp As Integer, ByVal hitPlace() As Boolean, ByRef powerfulHit As Boolean, ByRef enemyStamina As Integer) As Boolean()
        'hitPlace() = {голова, живот, правая рука, левая рука, правая нога, левая нога}
        Dim rndNum As Integer

        For i = 0 To UBound(hitPlace)
            hitPlace(i) = False
        Next

        If playerHp >= 50 Then
            If enemyHp >= 50 Then
                rndNum = Rnd() * (2 - 1) + 1
                Select Case rndNum
                    Case 1
                        'Удар по голове
                        If enemyStamina >= 7 Then
                            powerfulHit = True
                        End If
                        hitPlace(0) = True
                    Case 2
                        'Удар в живот
                        If enemyStamina >= 7 Then
                            powerfulHit = True
                        End If
                        hitPlace(1) = True
                End Select
            Else
                rndNum = Rnd() * (2 - 1) + 1
                Select Case rndNum
                    Case 1
                        hitPlace(2) = True
                    Case 2
                        hitPlace(3) = True
                End Select
            End If
        Else
            If enemyHp < 50 Then
                rndNum = Rnd() * (6 - 1) + 1
                If rndNum >= 4 Then
                    If rndNum = 4 Then
                        hitPlace(5) = True
                    Else
                        hitPlace(6) = True
                    End If
                Else
                    If rndNum >= 2 Then
                        'Удар по голове
                        If enemyStamina >= 7 Then
                            powerfulHit = True
                        End If
                        hitPlace(0) = True
                    Else
                        'Удар в живот
                        If enemyStamina >= 7 Then
                            powerfulHit = True
                        End If
                        hitPlace(1) = True
                    End If
                End If
            Else
                rndNum = Rnd() * (1 - 0) + 1
                If rndNum = 0 Then
                    'Удар по голове
                    hitPlace(0) = True
                Else
                    'Удар в живот
                    hitPlace(1) = True
                End If
            End If
        End If

        Return hitPlace
    End Function
    Sub Greetings(m() As String, name As String, icon() As String, enemy As String)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        Dim str As String

        Dim speeches() As String =
            {
                "*THE KNIGHT STANDS UP YOU ON THE WAY*",
                "*THE MAGICIAN APPEARS IN FRONT OF YOU*",
                "*MR SLIME CREWS DIRECT TO YOU*",
                "*THE MAGIC CHEST JUST LIES IN FRONT OF YOU*"
            }

        Dim speeches2() As String =
            {
                "NO TIME TO WAIT! IN THE NAME OF LORD BARINGTON!",
                "LET THE POWER OF DALARAN COME WITH ME ...",
                "MR SLIME IS INTERESTINGLY TURNING IN FRONT OF YOU.",
                "*SUDDENLY THE CHEST RISE 3 METERS INTO THE AIR*"
            }

        'Вывод иконки противника
        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(icon, 30, 14, speed - 0.001)
        Console.ResetColor()
        'Вывод диалогового окна
        DisplayingPictureFromSide(DisplayingDialogBox("simpleDialogWindow"), 46, 14, speed - 0.001)

        'Вывод имени противника
        str = enemy
        LineOutPutCharacterByCharacter(str, 49, 15, speed + 0.01, ConsoleColor.DarkYellow)
        'Вывод речи противника
        Select Case name
            Case "knight"
                str = speeches(0)
            Case "magician"
                str = speeches(1)
            Case "mrSlime"
                str = speeches(2)
            Case "magicchest"
                str = speeches(3)
        End Select
        LineOutPutCharacterByCharacter(str, 49, 17, speed + 0.01, ConsoleColor.White)
        Pause(2)
        Select Case name
            Case "knight"
                str = speeches2(0)
            Case "magician"
                str = speeches2(1)
            Case "mrSlime"
                str = speeches2(2)
            Case "magicchest"
                str = speeches2(3)
        End Select
        LineOutPutCharacterByCharacter(str, 49, 19, speed + 0.01, ConsoleColor.White)

    End Sub
    Sub IconsOfPleyers(icon() As String)
        'Вывод иконки противника
        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(icon, 5, 2, speed)
        'Вывод иконка игрока
        DisplayingPictureFromSide(IssuingAnIcon("mrpip"), 121, 2, speed)
        Console.ResetColor()
    End Sub
    Sub PlayersHp(enemyHp As Integer, enemyStamina As Integer)
        Dim hpBar() As String =
            {
                "═══════════════════════════════════║",'l = 34
                "║                                  ║",
                "║                                  ║",
                "║                                  ║",
                "║                                  ║",
                "║═══════════════════════════════════"
            }

        Dim hpPoint() As String =
            {
                "█",
                "█",
                "█",
                "█"
            }

        Dim l As Integer

        Dim hp

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.0017
        End If

        'Вывод здоровья противника
        Console.ForegroundColor = ConsoleColor.Yellow
        DisplayingPictureFromSide(hpBar, 22, 3, speed)
        Console.ResetColor()
        Console.SetCursorPosition(20, 1)
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write("[ ENEMY HP = {0} ] [ ENEMY STAMINA = {1}]", enemyHp, enemyStamina)
        Console.ResetColor()
        'Вывод здоровья игрока
        Console.ForegroundColor = ConsoleColor.DarkCyan
        DisplayingPictureFromSide(hpBar, 81, 3, speed)
        Console.ResetColor()
        Console.SetCursorPosition(78, 1)
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write("[ PLAYER HP = {0} ] [ PLAYER STAMINA = {1}]", playerHp, playerStamina)
        Console.ResetColor()

        For i = 0 To 1
            If kingdom_of_snails Then
                speed = 0
            Else
                speed = 0.0035
            End If
            If i = 0 Then
                hp = enemyHp
                l = 24
            Else
                hp = playerHp
                l = 83
            End If
            If hp > 100 Then
                Console.ForegroundColor = ConsoleColor.Green
                For j = 1 To 32
                    DisplayingPictureFromSide(hpPoint, l, 4, speed)
                    l += 1
                Next
                Console.ResetColor()
            ElseIf hp = 100 Then
                Console.ForegroundColor = ConsoleColor.Green
                For j = 1 To 29
                    DisplayingPictureFromSide(hpPoint, l, 4, speed)
                    l += 1
                Next
                Console.ResetColor()
            ElseIf hp > 50 And hp < 100 Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
                For j = 1 To 26
                    DisplayingPictureFromSide(hpPoint, l, 4, speed)
                    l += 1
                Next
                Console.ResetColor()
            ElseIf hp = 50 Then
                Console.ForegroundColor = ConsoleColor.DarkYellow
                For j = 1 To 15
                    DisplayingPictureFromSide(hpPoint, l, 4, speed)
                    l += 1
                Next
                Console.ResetColor()
            ElseIf hp < 50 Then
                Console.ForegroundColor = ConsoleColor.Red
                For j = 1 To 7
                    DisplayingPictureFromSide(hpPoint, l, 4, speed)
                    l += 1
                Next
                Console.ResetColor()
            End If
        Next

    End Sub
    Sub Poster_ancient_evil_is_invincible(special_num As Byte)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        Dim poster_ancient_exil_is_invincible() As String =
            {
                "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────",
                "│                 ╔═══╗╔╗ ╔╗╔══╗ ╔═══╗╔╗ ╔╗╔═══╗ ╔════╗╔╗ ╔╗╔═══╗╔══╗╔═══╗ ╔═══╗╔═══╗╔╗ ╔╗╔╗   ╔═══╗                 │",
                "│                 ║╔═╗║║║ ║║║╔╗║ ╚╗╔╗║║║ ║║║╔══╝ ║╔╗╔╗║║║ ║║║╔══╝╚╣╠╝║╔═╗║ ║╔═╗║║╔═╗║║║ ║║║║   ║╔═╗║                 │",
                "│─────────────────║╚══╗║║ ║║║╚╝╚╗ ║║║║║║ ║║║╚══╗ ╚╝║║╚╝║╚═╝║║╚══╗ ║║ ║╚═╝║ ║╚══╗║║ ║║║║ ║║║║   ║╚══╗─────────────────│",
                "│  │   │   │   │  ╚══╗║║║ ║║║╔═╗║ ║║║║║║ ║║║╔══╝   ║║  ║╔═╗║║╔══╝ ║║ ║╔╗╔╝ ╚══╗║║║ ║║║║ ║║║║ ╔╗╚══╗║  │   │   │   │  │",
                "│─────────────────║╚═╝║║╚═╝║║╚═╝║╔╝╚╝║║╚═╝║║╚══╗   ║║  ║║ ║║║╚══╗╔╣╠╗║║║╚╗ ║╚═╝║║╚═╝║║╚═╝║║╚═╝║║╚═╝║─────────────────│",
                "│                 ╚═══╝╚═══╝╚═══╝╚═══╝╚═══╝╚═══╝   ╚╝  ╚╝ ╚╝╚═══╝╚══╝╚╝╚═╝ ╚═══╝╚═══╝╚═══╝╚═══╝╚═══╝                 │",
                "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────",
                "║   ║║                     ║║        ║│  │║                                ║│  │║        ║║                     ║║   ║",
                "║   ║╬═════════════════════╬║        ║│  │║                                ║│  │║        ║╬═════════════════════╬║   ║",
                "║   ╬═══════════════════════╬        ║│  │║                                ║│  │║        ╬═══════════════════════╬   ║",
                "║   ║│         ┼─          │║        ║│  │║                                ║│  │║        ║│                     │║   ║",
                "║═══║│        ┼┼──  ╬      │║════════════════════════════════════════════════════════════║│  ══════════════║══  │║═══║",
                "║   ║│    ╬  ┼─██┼─        │║                                                            ║│    ║┌────┐─────║    │║   ║",
                "║   ║│      ┼─████┼─       │║                                                            ║│    ║│┌┐┌┐│─────║    │║   ║",
                "║═══║│      ┼┼████┼┼ ╬     │║════════════════════════════════════════════════════════════║│    ║└┘││└┘─────║    │║═══║",
                "║   ║│   ╬  ─┼████─┼       │║                                                            ║│    ║──││─┌───┐─║    │║   ║",
                "║   ║│       ─┼██─┼        │║                            ╬  ╬                            ║│    ║──││─│┌─┐│─║    │║   ║",
                "║   ║│     ╬  ──┼┼   ╬     │║                            ║══║                            ║│    ║──└┘─│└──┐─║    │║   ║",
                "║   ║│         ─┼  ╬       │║                        ╬ ╬║    ║╬ ╬                        ║│  ════════└──┐│═║══  │║   ║",
                "║   ║│                     │║                        ║ ║│    │║ ║                        ║│    ║─────│└─┘│─║    │║   ║",
                "║   ║│  │──────│─│──────│  │║                        ║║│      │║║                        ║│    ║─────└───┘─║    │║   ║",
                "║   ║│                     │║                        ║│        │║                        ║│                     │║   ║",
                "║   ║│  │╬════╬│           │║       ╬══════╬         ║│        │║         ╬══════╬       ║│  │╬════╬│           │║   ║",
                "║   ║│  │║    ║│ │╬════╬│  │║        ╬════╬          ║│        │║          ╬════╬        ║│  │║    ║│ │╬════╬│  │║   ║",
                "║   ╬════╬    ╬═══╬    ╬════╬         ╬══╬           ║│        │║           ╬══╬         ╬════╬    ╬═══╬    ╬════╬   ║",
                "║                                      ║║            ║│        │║            ║║                                      ║",
                "║                                      ║║            ║│        │║            ║║                                      ║",
                "║                                      ║║           ╬══╬      ╬══╬           ║║                                      ║",
                "║  ╬═══╬      ╬═══╬      ╬═══╬         ║║           ║  ║      ║  ║           ║║         ╬═══╬      ╬═══╬      ╬═══╬  ║",
                "║   ╬═╬        ╬═╬        ╬═╬          ║║           ╬══╬      ╬══╬           ║║          ╬═╬        ╬═╬        ╬═╬   ║",
                "║    ║          ║          ║           ║║        │──────────────────│        ║║           ║          ║          ║    ║",
                "║   ═══        ═══        ═══          ║║      │──────────────────────│      ║║          ═══        ═══        ═══   ║",
                "║    ║          ║          ║           ║║    │──────────────────────────│    ║║           ║          ║          ║    ║",
                "║                                   │──║───│──────────────────────────────│──║───│                                   ║",
                "══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════"
            }

        Console.SetCursorPosition(0, 2)
        Console.ForegroundColor = ConsoleColor.Magenta
        ImageOutputLineByLine(poster_ancient_exil_is_invincible, 0.04, 11)
        Console.ResetColor()

        Console.ReadKey()

        If special_num = 0 Then
            mainMenu()
        Else
            show_posters()
        End If
    End Sub
    Sub Poster_prophecy_the_dungeon_silent_temple(special_num As Byte)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        Dim poster_prophecy_the_dungeon_silent_temple() As String =
            {
                "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────",
                "│                      ╔═══╗╔═══╗╔═══╗╔═══╗ ╔════╗╔╗ ╔╗╔═══╗╔══╗╔═══╗ ╔═══╗╔═══╗╔╗ ╔╗╔╗   ╔═══╗                      │",
                "│                      ║╔══╝║╔═╗║║╔══╝║╔══╝ ║╔╗╔╗║║║ ║║║╔══╝╚╣╠╝║╔═╗║ ║╔═╗║║╔═╗║║║ ║║║║   ║╔═╗║                      │",
                "│──────────────────────║╚══╗║╚═╝║║╚══╗║╚══╗ ╚╝║║╚╝║╚═╝║║╚══╗ ║║ ║╚═╝║ ║╚══╗║║ ║║║║ ║║║║   ║╚══╗──────────────────────│",
                "│  │   │   │   │   │   ║╔══╝║╔╗╔╝║╔══╝║╔══╝   ║║  ║╔═╗║║╔══╝ ║║ ║╔╗╔╝ ╚══╗║║║ ║║║║ ║║║║ ╔╗╚══╗║   │   │   │   │   │  │",
                "│──────────────────────║║   ║║║╚╗║╚══╗║╚══╗   ║║  ║║ ║║║╚══╗╔╣╠╗║║║╚╗ ║╚═╝║║╚═╝║║╚═╝║║╚═╝║║╚═╝║──────────────────────│",
                "│                      ╚╝   ╚╝╚═╝╚═══╝╚═══╝   ╚╝  ╚╝ ╚╝╚═══╝╚══╝╚╝╚═╝ ╚═══╝╚═══╝╚═══╝╚═══╝╚═══╝                      │",
                "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────",
                "║                                                      ──────────│                              ║   ║                ║",
                "║                                                         ─────│                      ────      ║   ║                ║",
                "║      │      │          ┼─                                │─│                          ──────  ║   ║        ─────   ║",
                "║                      │─┼─                                 │─                              ─── ║   ║ ────           ║",
                "║                     │─│─│─        │                      ││                                   ║   ║                ║",
                "║           │        │─│  ─│─                               │                                   ║   ║                ║",
                "║                   │─│    ─│─          │                                                       ║   ║                ║",
                "║                  │─│      ─│─                              ╬                     ║══║         ║   ║                ║",
                "║                  ─│       ─│─│                         ╬                          ═════       ║ ╬ ║                ║",
                "║                  ─│─      │─│                     ╬                                           ║ ║ ║      ════║     ║",
                "║                   ─│─    │─│   │                     ─────│─────  ╬                           ║═║═║      ║════     ║",
                "║           │        ─│─  │─│                          │║═══│════│                            │═══════│              ║",
                "║                     ─│─│─│                           │════│════│                           ║═        ═╬║           ║",
                "║                      ─┼─                             │════│═══║│  ╬                       ═╬          ║║           ║",
                "║                      ─┼                              ─────│─────                         ║│            ║│          ║",
                "║         ╬═══╬           ║═╬      ╬═══╬             ╬                                     ║│            ║│          ║",
                "║          ╬═╬       ╬═║  ║╬        ╬═╬                     ╬                             ╬║│            ║│╬         ║",
                "║           ║         ╬════          ║                   ║═════║                   ╬═══╬  │║             ║│║  ╬═══╬  ║",
                "║          ═══          ║═          ═══                  ═══════                    ╬═╬   │║             ║│║   ╬═╬   ║",
                "║           ║█          ║│          █║                  ═════════                    ║    │║             ║│║    ║    ║",
                "║═════════██║███════════│═════════███║██═════════════════║     ║══════════════════════════│║             ║│║═════════║",
                "║        ║═████═║       ││       ║═████═║                ║     ║                     ║    │║             ║│║    ║    ║",
                "║═════════║═██═║║════════│════════║═██═║║════════════════║     ║══════════════════════════│║             ║│║═════════║",
                "║         ║══════║      ││        ║══════║               ║     ║                          │║             ║│║╬        ║",
                "║        ║║║═════║      │═       ║║║═════║               ║     ║                          │║║            ║│═║        ║",
                "║       ║══════════║    ││    ║║═══════════║             ║     ║                          │║║            ║│═║        ║",
                "║      ║══════════║║    ═│    ║║║═══════════║            ═══════                          │║║            ║│═║        ║",
                "║      ║══════════║║    ║│    ║║║═══════════║        ═══════════════                      │║║─────││─────║│║══       ║",
                "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────"
            }

        Console.SetCursorPosition(0, 2)
        Console.ForegroundColor = ConsoleColor.Cyan
        ImageOutputLineByLine(poster_prophecy_the_dungeon_silent_temple, 0.04, 11)
        Console.ResetColor()

        Console.ReadKey()
        If special_num = 0 Then
            mainMenu()
        Else
            show_posters()
        End If
    End Sub
    Sub Poster_accept_your_destiny(special_num As Byte)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        Dim poster_accept_your_destiny() As String =
            {
                "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────",
                "│                  ╔══╗╔══╗╔══╗╔═══╗╔═══╗╔════╗ ╔╗╔╗╔══╗╔╗╔╗╔═══╗ ╔══╗ ╔═══╗╔══╗╔════╗╔══╗╔╗─╔╗╔╗╔╗                  │",
                "│                  ║╔╗║║╔═╝║╔═╝║╔══╝║╔═╗║╚═╗╔═╝ ║║║║║╔╗║║║║║║╔═╗║ ║╔╗╚╗║╔══╝║╔═╝╚═╗╔═╝╚╗╔╝║╚═╝║║║║║                  │",
                "│──────────────────║╚╝║║║  ║║  ║╚══╗║╚═╝║  ║║   ║╚╝║║║║║║║║║║╚═╝║ ║║╚╗║║╚══╗║╚═╗  ║║   ║║ ║╔╗ ║║╚╝║──────────────────│",
                "│  │   │   │   │   ║╔╗║║║  ║║  ║╔══╝║╔══╝  ║║   ╚═╗║║║║║║║║║║╔╗╔╝ ║║ ║║║╔══╝╚═╗║  ║║   ║║ ║║╚╗║╚═╗║   │   │   │   │  │",
                "│──────────────────║║║║║╚═╗║╚═╗║╚══╗║║     ║║    ╔╝║║╚╝║║╚╝║║║║║  ║╚═╝║║╚══╗╔═╝║  ║║  ╔╝╚╗║║ ║║ ╔╝║──────────────────│",
                "│                  ╚╝╚╝╚══╝╚══╝╚═══╝╚╝     ╚╝    ╚═╝╚══╝╚══╝╚╝╚╝  ╚═══╝╚═══╝╚══╝  ╚╝  ╚══╝╚╝ ╚╝ ╚═╝                  │",
                "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────",
                "║    ║═════════════║           ║═════════════║               ┼          ║═════════════║           ║═════════════║    ║",
                "║      ║═════════║               ║═════════║          ┼                   ║═════════║               ║═════════║      ║",
                "║       │║     │║                 │║     │║                                │║     │║                 │║     │║       ║",
                "║       │║     │║                 │║     │║              │  │   ┼          │║     │║                 │║     │║       ║",
                "║       │║     │║                 │║     │║       ┼    │ │  │ │    ┼       │║ │   │║                 │║     │║       ║",
                "║       │║     │║══║═════║═════║══│║     │║            │─│──│─│            │║ │   │║══║═════║═════║══│║     │║       ║",
                "║       │║     │║ ║║║    ║    ║║║ │║     │║            ────────            │║     │║ ║║║    ║    ║║║ │║     │║       ║",
                "║       │║     │║  ╬═══════════╬  │║     │║         ┼           ┼          │║     │║  ╬═══════════╬  │║     │║       ║",
                "║       │║     │║  ║           ║  │║     │║                                │║     │║  ║           ║  │║     │║       ║",
                "║       │║     │║  ║           ║  │║     │║           ║════════║           │║     │║  ║           ║  │║     │║       ║",
                "║       │║     │║  ║     │     ║  │║     │║           ╬═║════║═╬           │║     │║  ║     │     ║  │║     │║       ║",
                "║       │║     │║  ║   ──│──   ║  │║ │   │║           │ ║    ║ │           │║     │║  ║   ──┼──   ║  │║     │║       ║",
                "║       │║     │║  ║     │     ║  │║ │   │║           │ ║    ║ │           │║     │║  ║     │     ║  │║     │║       ║",
                "║       │║     │║  ║     │     ║  │║ │   │║           │ ║    ║             │║     │║  ║     │     ║  │║     │║       ║",
                "║       │║     │║  ║     │     ║  │║     │║           │ ║    ║             │║ │   │║  ║     │     ║  │║     │║       ║",
                "║       │║     │║  ║   ──┼──   ║  │║     │║             ║    ║║            │║ │   │║  ║   ──│──   ║  │║     │║       ║",
                "║       │║     │║  ║     │     ║  │║     │║             ║     ║            │║ │   │║  ║     │     ║  │║     │║       ║",
                "║       │║     │║  ║           ║  │║     │║             ║     │║           │║     │║  ║           ║  │║     │║       ║",
                "║       │║     │║══║═══════════║══│║   │ │║             ║║    │║           │║     │║══║═══════════║══│║     │║       ║",
                "║       │║     │║ ║║║   ║║║   ║║║ │║   │ │║             ║║    │║║          │║     │║ ║║║   ║║║   ║║║ │║     │║       ║",
                "║       │║     │║  ╬     ╬     ╬  │║     │║             ║║    ││║║         │║    ││║  ╬     ╬     ╬  │║     │║       ║",
                "║       │║     │║                 │║     │║             ║║    │││║         │║    ││║                 │║     │║       ║",
                "║       │║     │║                 │║     │║            ║║║    │││║║        │║     │║                 │║     │║       ║",
                "║       │║     │║                 │║     │║           ║║║║════│││║║        │║     │║                 │║     │║       ║",
                "║       │║     │║                 │║     │║          ║║║║═════││││║        │║     │║                 │║     │║       ║",
                "║      ║═════════║               ║═════════║        ║║║║║═════││││║       ║═════════║               ║═════════║      ║",
                "║    ║═════════════║           ║═════════════║     ║║║║║══════││││║║    ║═════════════║           ║═════════════║    ║",
                "══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════"
            }

        Console.SetCursorPosition(0, 2)
        Console.ForegroundColor = ConsoleColor.DarkYellow
        ImageOutputLineByLine(poster_accept_your_destiny, 0.04, 11)
        Console.ResetColor()

        Console.ReadKey()
        If special_num = 0 Then
            mainMenu()
        Else
            show_posters()
        End If
    End Sub

    Sub Congrats_posters(chapter_num As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        Dim conrats_pic() As String =
            {
                " █████╗  █████╗ ███╗  ██╗ ██████╗ ██████╗  █████╗ ████████╗██╗   ██╗██╗      █████╗ ████████╗██╗ █████╗ ███╗  ██╗ ██████╗",
                "██╔══██╗██╔══██╗████╗ ██║██╔════╝ ██╔══██╗██╔══██╗╚══██╔══╝██║   ██║██║     ██╔══██╗╚══██╔══╝██║██╔══██╗████╗ ██║██╔════╝",
                "██║  ╚═╝██║  ██║██╔██╗██║██║  ██╗ ██████╔╝███████║   ██║   ██║   ██║██║     ███████║   ██║   ██║██║  ██║██╔██╗██║╚█████╗ ",
                "██║  ██╗██║  ██║██║╚████║██║  ╚██╗██╔══██╗██╔══██║   ██║   ██║   ██║██║     ██╔══██║   ██║   ██║██║  ██║██║╚████║ ╚═══██╗",
                "╚█████╔╝╚█████╔╝██║ ╚███║╚██████╔╝██║  ██║██║  ██║   ██║   ╚██████╔╝███████╗██║  ██║   ██║   ██║╚█████╔╝██║ ╚███║██████╔╝",
                " ╚════╝  ╚════╝ ╚═╝  ╚══╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝ ╚════╝ ╚═╝  ╚══╝╚═════╝ "
            }

        Dim chapter_pic() As String =
            {
                "╔══╗╔╗╔╗╔══╗╔═══╗╔════╗╔═══╗╔═══╗",
                "║╔═╝║║║║║╔╗║║╔═╗║╚═╗╔═╝║╔══╝║╔═╗║",
                "║║  ║╚╝║║╚╝║║╚═╝║  ║║  ║╚══╗║╚═╝║",
                "║║  ║╔╗║║╔╗║║╔══╝  ║║  ║╔══╝║╔╗╔╝",
                "║╚═╗║║║║║║║║║║     ║║  ║╚══╗║║║║ ",
                "╚══╝╚╝╚╝╚╝╚╝╚╝     ╚╝  ╚═══╝╚╝╚╝ "
            }

        Dim complt_pic() As String =
            {
                "╔══╗╔══╗╔╗  ╔╗╔═══╗╔╗  ╔═══╗╔════╗╔═══╗╔══╗ ",
                "║╔═╝║╔╗║║║  ║║║╔═╗║║║  ║╔══╝╚═╗╔═╝║╔══╝║╔╗╚╗",
                "║║  ║║║║║╚╗╔╝║║╚═╝║║║  ║╚══╗  ║║  ║╚══╗║║╚╗║",
                "║║  ║║║║║╔╗╔╗║║╔══╝║║  ║╔══╝  ║║  ║╔══╝║║ ║║",
                "║╚═╗║╚╝║║║╚╝║║║║   ║╚═╗║╚══╗  ║║  ║╚══╗║╚═╝║",
                "╚══╝╚══╝╚╝  ╚╝╚╝   ╚══╝╚═══╝  ╚╝  ╚═══╝╚═══╝"
            }

        Dim num_one_pic() As String =
            {
                "       ███╗       ",
                "      ████║       ",
                "║════██═██║══════║",
                "║═══════██║══════║",
                "     ███████╗     ",
                "     ╚══════╝     "
            }

        Dim num_two_pic() As String =
            {
                "     ██████╗ ",
                "     ╚════██╗",
                "║══════███╔═╝════║",
                "║════██╔══╝══════║",
                "     ███████╗",
                "     ╚══════╝"
            }

        Dim num_three_pic() As String =
            {
                "     ██████╗      ",
                "     ╚════██╗     ",
                "║═════█████╔╝════║",
                "║═════════██╗════║",
                "     ██████╔╝     ",
                "     ╚═════╝      "
            }

        Dim str5() As String =
            {
                "═════╗",
                "║╔╗╔╗║",
                "╚╝ ╚═╝"
            }

        Dim info_for_user() As String =
            {
                "[ ALL POSTERS SAVED IN THE (TAVERN / ACHIEVEMENTS / USER POSTERS) ]",
                "[ ENTER ""1"" TO VIEW POSTER ]",
                "[ ENTER ""0"" (ZERO) TO EXIT THE MAIN MENU ]"
            }

        Dim l As Integer

        Dim str, userAns As String

        speed = 0.0038

        Do
            Console.SetCursorPosition(0, 5)
            Console.ForegroundColor = ConsoleColor.White
            ImageOutputLineByLine(conrats_pic, 0.04, 11)
            Console.ResetColor()

            'Вывод линии соединения цветков |Верхний левый угол - Верхний правый угол|
            l = 11
            Console.ForegroundColor = ConsoleColor.DarkYellow
            For i = 1 To 24
                DisplayingPictureFromSide(str5, l, 1, 0.0019)
                l += 5
            Next
            Console.ResetColor()

            'Вывод полос слева
            l = 1
            Console.ForegroundColor = ConsoleColor.Yellow
            For i = 1 To 38
                Console.SetCursorPosition(2, l)
                Console.Write("║╔║║╗║")
                l += 1
                Pause(0.01)
            Next
            Console.ResetColor()

            l = 1
            'Вывод полос справа
            Console.ForegroundColor = ConsoleColor.Yellow
            For i = 1 To 38
                Console.SetCursorPosition(134, l)
                Console.Write("║║║║║║")
                l += 1
                Pause(0.01)
            Next
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(chapter_pic, 19, 17, speed)
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Pause(0.5)
            Select Case chapter_num
                Case 1
                    DisplayingPictureFromSide(num_one_pic, 27, 25, speed + 0.0001)
                Case 2
                    DisplayingPictureFromSide(num_two_pic, 27, 25, speed + 0.0001)
                Case 3
                    DisplayingPictureFromSide(num_three_pic, 27, 25, speed + 0.0001)
            End Select
            Console.ResetColor()
            Pause(0.5)
            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(complt_pic, 14, 33, speed)
            Console.ResetColor()

            'Вывод сообщения пользователю
            str = "[ YOU RECEIVED A POSTER: ]"
            LineOutPutCharacterByCharacter(str, 64, 17, 0.008, ConsoleColor.Magenta)
            Select Case chapter_num
                Case 1
                    str = """FREE THEIR SOULS"""
                Case 2
                    str = """SUBDUE THEIR SOULS"""
                Case 3
                    str = """ACCEPT YOUR DESTINY"""
            End Select
            Pause(1.5)
            LineOutPutCharacterByCharacter(str, 92, 17, 0.008, ConsoleColor.DarkYellow)

            Pause(1)

            l = 21
            For i = 0 To UBound(info_for_user)
                str = info_for_user(i)
                LineOutPutCharacterByCharacter(str, 64, l, 0.004, ConsoleColor.Yellow)

                l += 2
            Next

            Console.SetCursorPosition(64, l + 3)
            Console.Write("-> ")

            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(userAns, 0, 1)

            If result Then
                Select Case userAns
                    Case "0"
                        mainMenu()
                    Case "1"
                        'Переход в процедуру показа постера
                        Select Case chapter_num
                            Case 1
                                Poster_prophecy_the_dungeon_silent_temple(0)
                            Case 2
                                Poster_ancient_evil_is_invincible(0)
                            Case 3
                                Poster_accept_your_destiny(0)
                        End Select
                End Select
            Else
                str = "[ INVALID VALUE ]"
                LineOutPutCharacterByCharacter(str, 64, l + 3, 0.004, ConsoleColor.Red)
                speed = 0
                Pause(1)
                Console.Clear()
            End If
        Loop
    End Sub
    Sub GameOver(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 100
        Console.SetBufferSize(100, 40)
        Console.Clear()

        speed = 0.0035

        Dim gameOverPic() As String =
            {
                " ██████╗  █████╗ ███╗   ███╗███████╗   █████╗ ██╗   ██╗███████╗██████╗ ",
                "██╔════╝ ██╔══██╗████╗ ████║██╔════╝  ██╔══██╗██║   ██║██╔════╝██╔══██╗",
                "██║  ██╗ ███████║██╔████╔██║█████╗    ██║  ██║╚██╗ ██╔╝█████╗  ██████╔╝",
                "██║  ██╗ ██╔══██║██║╚██╔╝██║██╔══╝    ██║  ██║ ╚████╔╝ ██╔══╝  █╔══██╗ ",
                "╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗  ╚█████╔╝  ╚██╔╝  ███████╗██║  ██║",
                " ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝   ╚════╝    ╚═╝   ╚══════╝╚═╝  ╚═╝"
            }

        Dim str As String

        Dim l As Integer

        If spNum = 1 Then

        Else
            Console.SetCursorPosition(0, 10)
            Console.ForegroundColor = ConsoleColor.Green
            ImageOutputLineByLine(gameOverPic, 0.04, 16)
            Console.ResetColor()
            str = "[ EARLY LEVEL COMPLETION ]"
            LineOutPutCharacterByCharacter(str, 16, 20, 0.04, ConsoleColor.Green)
            Pause(2)
            'START MODE
            str = "-> START MODE"
            LineOutPutCharacterByCharacter(str, 16, 22, 0.04, ConsoleColor.Green)
            Pause(1)
            str = "[ COUNTDOWN TO RETURN ]"
            LineOutPutCharacterByCharacter(str, 16, 24, 0.04, ConsoleColor.Green)
            l = 26
            Console.ForegroundColor = ConsoleColor.Green
            For i = 1 To 5
                Console.SetCursorPosition(16, l)
                Console.Write("//{0}", i)
                l += 2
                Pause(1)
            Next
            Console.ResetColor()
        End If

        pathMenu()
    End Sub
    Sub StandartGameMode(m() As String, name As String, icon() As String, enemy As String, chapter_num As Integer, levelNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        Dim num1() As String =
            {
                " ╔╗  ",
                "╔╝║  ",
                "╚╗║  ",
                " ║║  ",
                "╔╝╚╗ ",
                "╚══╝ "
            }
        Dim num2() As String =
            {
                "╔═══╗",
                "║╔═╗║",
                "╚╝╔╝║",
                "╔═╝╔╝",
                "║ ╚═╗",
                "╚═══╝"
            }

        Dim num3() As String =
            {
                "╔═══╗",
                "║╔═╗║",
                "╚╝╔╝║",
                "╔╗╚╗║",
                "║╚═╝║",
                "╚═══╝"
            }

        Dim gameStartsIn() As String =
            {
                "▀█▀ █░█ █▀▀   █▀▀ ▄▀█ █▀▄▀█ █▀▀   █▀ ▀█▀ ▄▀█ █▀█ ▀█▀ █▀   █ █▄░█",
                "░█░ █▀█ ██▄   █▄█ █▀█ █░▀░█ ██▄   ▄█ ░█░ █▀█ █▀▄ ░█░ ▄█   █ █░▀█"
            }

        Dim goPic() As String =
            {
                "╔═══╦ ╔═══╦╗",
                "║╔═╗║ ║╔═╗║║",
                "║║ ╚╣ ║║ ║║║",
                "║║╔═╣ ║║ ║╠╝",
                "║╚╩═║ ║╚═╝╠╗",
                "╚═══╩ ╚═══╩╝"
            }

        Dim shield() As String =
            {
                " ╬═════════╬ ",
                "╬     │     ╬",
                "║     │     ║",
                "║  │  │  │  ║",
                "║  ┼  │  ┼  ║",
                "║  │  │  │  ║",
                "║║    │    ║║",
                " ║║   │   ║║ ",
                "  ║║  │  ║║  ",
                "   ┼──┼──┼   "
            }

        Dim sword() As String =
            {
                "      ╬      ",
                "      ║      ",
                "  ╬   ║   ╬  ",
                "  ╬═══║═══╬  ",
                "      ║      ",
                "      ║      ",
                "  ╬   ║   ╬  ",
                "  ╬═══║═══╬  ",
                "      ║      ",
                "      ║      "
            }

        Dim protectionPic() As String =
            {
                "╔═══╦═══╦══╦════╦═══╦══╦════╦══╦══╦╗─╔╗",
                "║╔═╗║╔═╗║╔╗╠═╗╔═╣╔══╣╔═╩═╗╔═╩╗╔╣╔╗║╚═╝║",
                "║╚═╝║╚═╝║║║║─║║─║╚══╣║───║║──║║║║║║╔╗─║",
                "║╔══╣╔╗╔╣║║║─║║─║╔══╣║───║║──║║║║║║║╚╗║",
                "║║──║║║║║╚╝║─║║─║╚══╣╚═╗─║║─╔╝╚╣╚╝║║─║║",
                "╚╝──╚╝╚╝╚══╝─╚╝─╚═══╩══╝─╚╝─╚══╩══╩╝─╚╝"
            }

        Dim attackPic() As String =
            {
                "╔══╦════╦════╦══╦══╦╗╔══╗",
                "║╔╗╠═╗╔═╩═╗╔═╣╔╗║╔═╣║║╔═╝",
                "║╚╝║─║║───║║─║╚╝║║─║╚╝║  ",
                "║╔╗║─║║───║║─║╔╗║║─║╔╗║  ",
                "║║║║─║║───║║─║║║║╚═╣║║╚═╗",
                "╚╝╚╝─╚╝───╚╝─╚╝╚╩══╩╝╚══╝"
            }

        Dim placesForBlows() As String =
            {
                "HEAD",
                "BELLY",
                "RIGHT HAND",
                "LEFT HAND",
                "RIGHT LEG",
                "LEFT HAND"
            }

        Dim enemyProtectM(1) As String
        Dim hitPlace(5) As Boolean
        Dim enemyHp, x, y, enemyStamina, k, num, damage, enemyProtectionPlace As Integer
        Dim rndNum As Byte
        Dim pass, powerfulHit, successfulDefense, successfulAttack As Boolean
        Dim str, userAns, maxDamage, second_user_ans As String

        userAns = ""
        str = ""
        enemyStamina = 14


        Randomize()
        Do
            enemyHp = Rnd() * (200 - 100) + 100
            If enemyHp Mod 5 = 0 Then
                pass = True
            End If
        Loop Until pass

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.0035
        End If
        Greetings(m, name, icon, enemy)

        Pause(1)
        Console.Clear()

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.0035
        End If

        'Старт стандартного мода
        maxDamage = 0

        For i = 0 To UBound(playerInventoryNames)
            Select Case playerInventoryNames(i)
                Case "BLUE SCARF"
                    maxDamage += 20
                Case "GOLDEN APPLE"
                    maxDamage += 10
                Case "CHAIN MAIL"
                Case "LAMB SWEATER"
                    maxDamage += 5
                Case "GREEN SHORTS"
                Case "WOODEN SWORD"
                    maxDamage += 15
                Case "BINOCULARS"
                    maxDamage += 10
                Case "IRON SWORD"
                    maxDamage += 20
                Case "DRUID'S STAFF"
                    maxDamage += 40
                Case "BOOK OF THE DAMNED"
                    maxDamage += 75
            End Select
        Next

        IconsOfPleyers(icon)
        PlayersHp(enemyHp, enemyStamina)

        'Вывод картинки противника
        Console.ForegroundColor = ConsoleColor.Cyan
        DisplayingPictureFromSide(m, 64, 2, speed)
        Console.ResetColor()

        'Вывод диалогового сообщения противника
        DisplayingPictureFromSide(DisplayingDialogBox("simpleDialogWindow"), 40, 20, speed)

        Randomize()
        rndNum = Rnd() * (4 - 1) + 1
        Select Case rndNum
            Case 1
                str = "LET'S FIND OUT WHAT YOU CAN DO!"
            Case 2
                str = "LET THE BATTLE BEGIN! IT WILL BE COOL..."
            Case 3
                str = "GOOD LUCK... PROBABLY... FORGET IT!"
            Case 4
                str = "glhf guy! I hope you know how it stands"
        End Select

        LineOutPutCharacterByCharacter(str, 50, 23, 0.01, ConsoleColor.DarkYellow)
        Pause(1.3)

        Do
            If kingdom_of_snails Then
                speed = 0
            Else
                speed = 0.0035
            End If
            k += 1
            If k Mod 2 <> 0 Then
                Do
                    speed = 0
                    'АТАКА ПРОТИВНИКА
                    Console.Clear()
                    IconsOfPleyers(icon)
                    PlayersHp(enemyHp, enemyStamina)
                    'Вывод картинки противника
                    Console.ForegroundColor = ConsoleColor.Cyan
                    DisplayingPictureFromSide(m, 64, 2, speed)
                    Console.ResetColor()
                    DisplayingPictureFromSide(DisplayingDialogBox("simpleDialogWindow"), 40, 20, speed)
                    'Вывод изображения щита
                    Console.ForegroundColor = ConsoleColor.Cyan
                    DisplayingPictureFromSide(shield, 20, 20, speed)
                    Console.ResetColor()
                    'Вывод изображения надписи
                    Console.ForegroundColor = ConsoleColor.Magenta
                    DisplayingPictureFromSide(protectionPic, 50, 13, speed)
                    Console.ResetColor()
                    Console.ForegroundColor = ConsoleColor.Cyan
                    DisplayingPictureFromSide(shield, 105, 20, speed)
                    Console.ResetColor()
                    If kingdom_of_snails Then
                        speed = 0
                    Else
                        speed = 0.0035
                    End If

                    'Вывод надписи
                    str = "WHERE DOES THE BLOW GO?"
                    LineOutPutCharacterByCharacter(str, 44, 22, speed + 0.04, ConsoleColor.Cyan)
                    'Вывод пунктов меню
                    Console.ForegroundColor = ConsoleColor.Yellow
                    For i = 0 To UBound(placesForBlows)
                        Select Case i
                            Case 0
                                x = 50
                                y = 24
                            Case 1
                                x = 50
                                y = 26
                            Case 2
                                x = 60
                                y = 24
                            Case 3
                                x = 60
                                y = 26
                            Case 4
                                x = 75
                                y = 24
                            Case 5
                                x = 75
                                y = 26
                        End Select
                        Console.SetCursorPosition(x, y)
                        Console.Write("{0}| ", i + 1)
                        Console.Write(placesForBlows(i))
                    Next
                    Console.ResetColor()
                    Console.SetCursorPosition(5, 38)
                    Console.ForegroundColor = ConsoleColor.Yellow
                    Console.Write("[ ENTER 0 TO STOP THE LEVEL EARLY ]")
                    Console.ResetColor()
                    Console.SetCursorPosition(60, 38)
                    Console.Write("-> ")
                    Console.ForegroundColor = ConsoleColor.Cyan
                    userAns = Console.ReadLine()
                    Console.ResetColor()
                    result = CheckUserAns(userAns, 0, 6)
                    If Not result Then
                        str = "[ INVALID VALUE ]"
                        LineOutPutCharacterByCharacter(str, 59, 38, speed + 0.002, ConsoleColor.Red)
                        speed = 0

                        Pause(1.2)
                    Else
                        If userAns = "0" Then
                            If playerCoins >= 20 Then
                                playerCoins -= 20
                            Else
                                If playerStamina >= 1 Then
                                    playerStamina -= 1
                                Else
                                    playerStamina = 0
                                End If
                            End If
                            GameOver(2)
                        Else
                            Exit Do
                        End If
                        Exit Do
                    End If
                Loop
            End If

            If k Mod 2 <> 0 Then
                'ЗАЩИТА ИГРОКА
                successfulDefense = False
                powerfulHit = False
                hitPlace = HitSelection(enemyHp, hitPlace, powerfulHit, enemyStamina)
                If powerfulHit Then
                    enemyStamina -= 7
                End If

                str = "[ PLAYER PROTECTION: ]"
                LineOutPutCharacterByCharacter(str, 55, 29, speed + 0.04, ConsoleColor.Magenta)
                Pause(1)
                Console.ForegroundColor = ConsoleColor.Cyan
                Select Case userAns
                    Case 1
                        Console.Write(" HEAD")
                    Case 2
                        Console.Write(" BELLY")
                    Case 3
                        Console.Write(" RIGHT HAND")
                    Case 4
                        Console.Write(" LEFT HAND")
                    Case 5
                        Console.Write(" RIGHT LEG")
                    Case 6
                        Console.Write(" LEFT LEG")
                End Select
                Console.ResetColor()
                Pause(1)
                str = "[ POWERFUL HIT: ]"
                LineOutPutCharacterByCharacter(str, 55, 31, speed + 0.04, ConsoleColor.Magenta)
                Pause(1)
                Console.ForegroundColor = ConsoleColor.Cyan
                If powerfulHit Then
                    Console.Write(" CHARGED")
                Else
                    Console.Write(" NOT CHARGED")
                End If
                Console.ResetColor()
                Pause(1)

                num = userAns

                str = "[ OPPONENT'S HIT: ]"
                LineOutPutCharacterByCharacter(str, 55, 33, speed + 0.04, ConsoleColor.Magenta)
                Pause(1)
                Console.ForegroundColor = ConsoleColor.Cyan

                For i = 0 To UBound(hitPlace)
                    If hitPlace(i) Then
                        If num - 1 = i Then
                            successfulDefense = True
                            Select Case i
                                Case 0
                                    If powerfulHit Then
                                        playerHp -= 30
                                        Console.Write(" HEAD")
                                        damage = 30
                                    Else
                                        Console.Write(" HEAD")
                                        damage = 0
                                    End If
                                Case 1
                                    If powerfulHit Then
                                        playerHp -= 25
                                        Console.Write(" BELLY")
                                        damage = 25
                                    Else
                                        Console.Write(" BELLY")
                                        damage = 0
                                    End If
                                Case 2
                                    If powerfulHit Then
                                        playerHp -= 20
                                        Console.Write(" RIGHT HAND")
                                        damage = 20
                                        Console.Write(" RIGHT HAND")
                                    Else
                                        damage = 0
                                    End If
                                Case 3
                                    If powerfulHit Then
                                        playerHp -= 20
                                        Console.Write(" LEFT HAND")
                                        damage = 20
                                    Else
                                        Console.Write(" LEFT HAND")
                                        damage = 0
                                    End If
                                Case 4
                                    If powerfulHit Then
                                        playerHp -= 20
                                        Console.Write(" RIGHT LEG")
                                        damage = 20
                                    Else
                                        Console.Write(" RIGHT LEG")
                                        damage = 0
                                    End If
                                Case 5
                                    If powerfulHit Then
                                        Console.Write(" LEFT LEG")
                                        playerHp -= 20
                                        damage = 20
                                    Else
                                        Console.Write(" LEFT LEG")
                                        damage = 0
                                    End If
                            End Select
                        Else
                            Select Case i
                                Case 0
                                    Console.Write(" HEAD")
                                    If powerfulHit Then
                                        playerHp -= 60
                                        damage = 60
                                    Else
                                        playerHp -= 40
                                        damage = 40
                                    End If
                                Case 1
                                    Console.Write(" BELLY")
                                    If powerfulHit Then
                                        playerHp -= 50
                                        damage = 50
                                    Else
                                        playerHp -= 30
                                        damage = 30
                                    End If
                                Case 2
                                    Console.Write(" RIGHT HAND")
                                    If powerfulHit Then
                                        playerHp -= 30
                                        damage = 30
                                    Else
                                        playerHp -= 20
                                        damage = 20
                                    End If
                                Case 3
                                    Console.Write(" LEFT HAND")
                                    If powerfulHit Then
                                        playerHp -= 30
                                        damage = 30
                                    Else
                                        playerHp -= 20
                                        damage = 20
                                    End If
                                Case 4
                                    Console.Write(" RIGHT LEG")
                                    If powerfulHit Then
                                        playerHp -= 30
                                        damage = 30
                                    Else
                                        playerHp -= 20
                                        damage = 20
                                    End If
                                Case 5
                                    Console.Write(" LEFT LEG")
                                    If powerfulHit Then
                                        playerHp -= 30
                                        damage = 30
                                    Else
                                        playerHp -= 20
                                        damage = 20
                                    End If
                            End Select
                        End If

                        If Not successfulDefense Then
                            str = "THERE IS A PUNCHING! = "
                            LineOutPutCharacterByCharacter(str, 55, 35, speed + 0.04, ConsoleColor.Magenta)
                            Pause(1)
                            Console.Write("[ - {0} HP ]", damage)
                            Console.ForegroundColor = ConsoleColor.Red
                            DisplayingPictureFromSide(shield, 20, 20, speed)
                            DisplayingPictureFromSide(shield, 105, 20, speed)
                            Console.ResetColor()
                        ElseIf damage > 0 Then
                            str = "THERE IS A PUNCHING! = "
                            LineOutPutCharacterByCharacter(str, 55, 35, speed + 0.04, ConsoleColor.Magenta)
                            Pause(1)
                            Console.Write("[ - {0} HP ]", damage)
                            Console.ForegroundColor = ConsoleColor.Green
                            DisplayingPictureFromSide(shield, 20, 20, speed)
                            DisplayingPictureFromSide(shield, 105, 20, speed)
                            Console.ResetColor()
                        End If
                    End If
                Next
            Else
                k = 0
                Do
                    k += 1
                    If k = 1 Then
                        'ЗАЩИТА ПРОТИВНИКА
                        Console.Clear()
                        IconsOfPleyers(icon)
                        PlayersHp(enemyHp, enemyStamina)
                        'Вывод картинки противника
                        Console.ForegroundColor = ConsoleColor.Cyan
                        DisplayingPictureFromSide(m, 64, 2, speed)
                        Console.ResetColor()
                        DisplayingPictureFromSide(DisplayingDialogBox("simpleDialogWindow"), 40, 20, speed)
                        'Вывод изображения меча
                        Console.ForegroundColor = ConsoleColor.Cyan
                        DisplayingPictureFromSide(sword, 20, 20, speed)
                        Console.ResetColor()
                        'Вывод изображения надписи
                        Console.ForegroundColor = ConsoleColor.Magenta
                        DisplayingPictureFromSide(attackPic, 57, 13, speed)
                        Console.ResetColor()
                        Console.ForegroundColor = ConsoleColor.Cyan
                        DisplayingPictureFromSide(sword, 105, 20, speed)
                        Console.ResetColor()
                        speed = 0.0035
                        'Вывод надписи
                        str = "WHERE DO YOU WANT TO HIT?"
                        LineOutPutCharacterByCharacter(str, 44, 22, speed + 0.04, ConsoleColor.Cyan)
                        'Вывод пунктов меню
                        Console.ForegroundColor = ConsoleColor.Yellow
                        For i = 0 To UBound(placesForBlows)
                            Select Case i
                                Case 0
                                    x = 50
                                    y = 24
                                Case 1
                                    x = 50
                                    y = 26
                                Case 2
                                    x = 60
                                    y = 24
                                Case 3
                                    x = 60
                                    y = 26
                                Case 4
                                    x = 75
                                    y = 24
                                Case 5
                                    x = 75
                                    y = 26
                            End Select

                            Console.SetCursorPosition(x, y)
                            Console.Write("{0}| ", i + 1)
                            Console.Write(placesForBlows(i))
                        Next
                        Console.ForegroundColor = ConsoleColor.Yellow
                        Console.SetCursorPosition(5, 38)
                        Console.Write("[ ENTER 0 TO STOP THE LEVEL EARLY ]")
                        Console.ResetColor()
                        Console.SetCursorPosition(60, 38)
                        Console.Write("-> ")
                        Console.ForegroundColor = ConsoleColor.Cyan
                        userAns = Console.ReadLine()
                        Console.ResetColor()
                        result = CheckUserAns(userAns, 0, 6)
                        If Not result Then
                            str = "[ INVALID VALUE ]"
                            LineOutPutCharacterByCharacter(str, 59, 38, speed + 0.002, ConsoleColor.Red)
                            speed = 0
                            k = 0
                            Pause(1.2)
                        Else
                            If userAns = "0" Then
                                If playerCoins >= 20 Then
                                    playerCoins -= 20
                                Else
                                    If playerStamina >= 1 Then
                                        playerStamina -= 1
                                    Else
                                        playerStamina = 0
                                    End If
                                End If
                                GameOver(2)
                            End If
                            If playerStamina >= 7 Then
                                k = 2
                            Else
                                powerfulHit = False
                            End If
                        End If
                    Else
                        Do
                            Console.Clear()
                            IconsOfPleyers(icon)
                            PlayersHp(enemyHp, enemyStamina)
                            'Вывод картинки противника
                            Console.ForegroundColor = ConsoleColor.Cyan
                            DisplayingPictureFromSide(m, 64, 2, speed)
                            Console.ResetColor()
                            DisplayingPictureFromSide(DisplayingDialogBox("simpleDialogWindow"), 40, 20, speed)
                            'Вывод изображения меча
                            Console.ForegroundColor = ConsoleColor.Cyan
                            DisplayingPictureFromSide(sword, 20, 20, speed)
                            Console.ResetColor()
                            'Вывод изображения надписи
                            Console.ForegroundColor = ConsoleColor.Magenta
                            DisplayingPictureFromSide(attackPic, 57, 13, speed)
                            Console.ResetColor()
                            Console.ForegroundColor = ConsoleColor.Cyan
                            DisplayingPictureFromSide(sword, 105, 20, speed)
                            Console.ResetColor()
                            speed = 0.0035
                            'Вывод надписи
                            If playerStamina = 0 Then
                                powerfulHit = False
                                Exit Do
                            Else
                                str = "USE A POWERFUL HIT?"
                                LineOutPutCharacterByCharacter(str, 44, 22, speed + 0.04, ConsoleColor.Cyan)
                                str = "1  -  YES|NO  -   2"
                                LineOutPutCharacterByCharacter(str, 57, 24, speed + 0.04, ConsoleColor.Yellow)
                                Console.ForegroundColor = ConsoleColor.Yellow
                                Console.SetCursorPosition(5, 38)
                                Console.Write("[ ENTER 0 TO STOP THE LEVEL EARLY ]")
                                Console.ResetColor()
                                Console.SetCursorPosition(60, 38)
                                Console.Write("-> ")
                                second_user_ans = Console.ReadLine()
                                Console.ResetColor()
                                result = CheckUserAns(second_user_ans, 0, 2)
                                If Not result Then
                                    str = "[ INVALID VALUE ]"
                                    LineOutPutCharacterByCharacter(str, 59, 38, speed + 0.002, ConsoleColor.Red)
                                    speed = 0
                                    k = 0
                                    Pause(1.2)
                                Else
                                    If second_user_ans = "0" Then
                                        If playerCoins >= 20 Then
                                            playerCoins -= 20
                                        Else
                                            If playerStamina >= 1 Then
                                                playerStamina -= 1
                                            Else
                                                playerStamina = 0
                                            End If
                                        End If
                                        GameOver(2)
                                    End If
                                    If playerStamina < 7 Then
                                        powerfulHit = False
                                        Exit Do
                                    Else
                                        If second_user_ans = "1" Then
                                            powerfulHit = True
                                            playerStamina -= 7
                                            Exit Do
                                        Else
                                            powerfulHit = False
                                            Exit Do
                                        End If
                                    End If
                                End If
                            End If
                        Loop

                        str = "[ PLAYER'S HIT: ]"
                        LineOutPutCharacterByCharacter(str, 55, 29, speed + 0.04, ConsoleColor.Magenta)
                        Pause(1)
                        Console.ForegroundColor = ConsoleColor.Cyan
                        Select Case userAns
                            Case 1
                                Console.Write(" HEAD")
                            Case 2
                                Console.Write(" BELLY")
                            Case 3
                                Console.Write(" RIGHT HAND")
                            Case 4
                                Console.Write(" LEFT HAND")
                            Case 5
                                Console.Write(" RIGHT LEG")
                            Case 6
                                Console.Write(" LEFT LEG")
                        End Select
                        Console.ResetColor()
                        Pause(1)
                        str = "[ POWERFUL HIT: ]"
                        LineOutPutCharacterByCharacter(str, 55, 31, speed + 0.04, ConsoleColor.Magenta)
                        Pause(1)
                        Console.ForegroundColor = ConsoleColor.Cyan
                        If powerfulHit Then
                            Console.Write(" CHARGED")
                        Else
                            Console.Write(" NOT CHARGED")
                        End If
                        Console.ResetColor()
                        str = "[ OPPONENT'S PROTECTION: ]"
                        LineOutPutCharacterByCharacter(str, 55, 33, speed + 0.04, ConsoleColor.Magenta)
                        Pause(1)
                        enemyProtectionPlace = EnemyProtection()
                        Console.ForegroundColor = ConsoleColor.Cyan
                        Select Case enemyProtectionPlace
                            Case 0
                                Console.Write(" HEAD")
                            Case 1
                                Console.Write(" BELLY")
                            Case 2
                                Console.Write(" RIGHT HAND")
                            Case 3
                                Console.Write(" LEFT HAND")
                            Case 4
                                Console.Write(" RIGHT LEG")
                            Case 5
                                Console.Write(" LEFT LEG")
                        End Select
                        Console.ResetColor()
                        damage = 0
                        If enemyProtectionPlace = userAns - 1 Then
                            successfulAttack = False
                            If powerfulHit Then
                                Select Case userAns
                                    Case 1
                                        damage = 30 + (maxDamage \ 2)
                                    Case 2
                                        damage = 25 + (maxDamage \ 2)
                                    Case 3
                                        damage = 20 + (maxDamage \ 2)
                                    Case 4
                                        damage = 20 + (maxDamage \ 2)
                                    Case 5
                                        damage = 20 + (maxDamage \ 2)
                                    Case 6
                                        damage = 20 + (maxDamage \ 2)
                                End Select
                            Else
                                Select Case userAns
                                    Case 1
                                        damage = 0
                                    Case 2
                                        damage = 0
                                    Case 3
                                        damage = 0
                                    Case 4
                                        damage = 0
                                    Case 5
                                        damage = 0
                                    Case 6
                                        damage = 0
                                End Select
                            End If
                        Else
                            successfulAttack = True
                            Select Case userAns
                                Case 1
                                    If powerfulHit Then
                                        damage = 60 + (maxDamage \ 2)
                                    Else
                                        damage = 50 + (maxDamage \ 2)
                                    End If
                                Case 2
                                    If powerfulHit Then
                                        damage = 50 + (maxDamage \ 2)
                                    Else
                                        damage = 30 + (maxDamage \ 2)
                                    End If
                                Case 3
                                    If powerfulHit Then
                                        damage = 30 + (maxDamage \ 2)
                                    Else
                                        damage = 20 + (maxDamage \ 2)
                                    End If
                                Case 4
                                    If powerfulHit Then
                                        damage = 30 + (maxDamage \ 2)
                                    Else
                                        damage = 20 + (maxDamage \ 2)
                                    End If
                                Case 5
                                    If powerfulHit Then
                                        damage = 30 + (maxDamage \ 2)
                                    Else
                                        damage = 20 + (maxDamage \ 2)
                                    End If
                                Case 6
                                    If powerfulHit Then
                                        damage = 30 + (maxDamage \ 2)
                                    Else
                                        damage = 20 + (maxDamage \ 2)
                                    End If
                            End Select
                        End If
                        enemyHp -= damage
                        If Not successfulAttack Then
                            str = "THERE IS A PUNCHING! = "
                            LineOutPutCharacterByCharacter(str, 55, 35, speed + 0.04, ConsoleColor.Magenta)
                            Pause(1)
                            Console.Write("[ - {0} HP ]", damage)
                            Console.ForegroundColor = ConsoleColor.Red
                            DisplayingPictureFromSide(sword, 20, 20, speed)
                            DisplayingPictureFromSide(sword, 105, 20, speed)
                            Console.ResetColor()
                        ElseIf damage > 0 Then
                            str = "THERE IS A PUNCHING! = "
                            LineOutPutCharacterByCharacter(str, 55, 35, speed + 0.04, ConsoleColor.Magenta)
                            Pause(1)
                            Console.Write("[ - {0} HP ]", damage)
                            Console.ForegroundColor = ConsoleColor.Green
                            DisplayingPictureFromSide(sword, 20, 20, speed)
                            DisplayingPictureFromSide(sword, 105, 20, speed)
                            Console.ResetColor()
                        End If
                        Exit Do
                    End If
                Loop
                k = 0
            End If
            str = "[ PRESS ENTER TO CONTINUE ]"
            LineOutPutCharacterByCharacter(str, 55, 38, speed, ConsoleColor.Yellow)
            Console.ReadKey()
        Loop Until (playerHp <= 0) Or (enemyHp <= 0)

        If playerHp > 0 Then
            standartWin(0, name, icon, m, chapter_num, levelNum)

            If Not done_first_season_tasks(10) Then
                For i = 0 To UBound(card_and_standart_games, 2)
                    If Not card_and_standart_games(1, i) Then
                        card_and_standart_games(1, i) = True
                        Exit For
                    End If
                Next
            End If
        ElseIf enemyHp > 0 Then
            standartWin(1, name, icon, m, chapter_num, levelNum)
        End If

    End Sub
    Sub StandartWin(spNum As Integer, enemyName As String, icon() As String, m() As String, chapter_num As Integer, levelNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 120
        Console.SetBufferSize(120, 40)
        Console.Clear()

        Dim gameOver() As String =
            {
                " ██████╗  █████╗ ███╗   ███╗███████╗   █████╗ ██╗   ██╗███████╗██████╗ ",
                "██╔════╝ ██╔══██╗████╗ ████║██╔════╝  ██╔══██╗██║   ██║██╔════╝██╔══██╗",
                "██║  ██╗ ███████║██╔████╔██║█████╗    ██║  ██║╚██╗ ██╔╝█████╗  ██████╔╝",
                "██  ╚██╗ ██╔══██║██║╚██╔╝██║██╔══╝    ██║  ██║ ╚████╔╝ ██╔══╝  ██╔══██╗",
                "╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗  ╚█████╔╝  ╚██╔╝  ███████╗██║  ██║",
                " ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝   ╚════╝    ╚═╝   ╚══════╝╚═╝  ╚═╝"
            }

        Dim youWonPic() As String =
            {
                "██╗   ██╗ █████╗ ██╗   ██╗ ██╗       ██╗ █████╗ ███╗  ██╗",
                "╚██╗ ██╔╝██╔══██╗██║   ██║ ██║  ██╗  ██║██╔══██╗████╗ ██║",
                " ╚████╔╝ ██║  ██║██║   ██║ ╚██╗████╗██╔╝██║  ██║██╔██╗██║",
                "  ╚██╔╝  ██║  ██║██║   ██║  ████╔═████║ ██║  ██║██║╚████║",
                "   ██║   ╚█████╔╝╚██████╔╝  ╚██╔╝ ╚██╔╝ ╚█████╔╝██║ ╚███║",
                "   ╚═╝    ╚════╝  ╚═════╝    ╚═╝   ╚═╝   ╚══╝ ╚═══╝  ╚══╝ "
            }

        Dim youLosePic() As String =
            {
                "██╗   ██╗ █████╗ ██╗   ██╗  ██╗      █████╗  ██████╗███████╗",
                "╚██╗ ██╔╝██╔══██╗██║   ██║  ██║     ██╔══██╗██╔════╝██╔════╝",
                " ╚████╔╝ ██║  ██║██║   ██║  ██║     ██║  ██║╚█████╗ █████╗  ",
                "  ╚██╔╝  ██║  ██║██║   ██║  ██║     ██║  ██║ ╚═══██╝██╔══╝   ",
                "   ██║   ╚█████╔╝╚██████╔╝  ███████╗╚█████╔╝██████╔ ███████╗",
                "   ╚═╝    ╚════╝  ╚═════╝   ╚══════╝ ╚════╝ ╚═════╝ ╚══════╝"
            }

        Dim infoTable() As String =
            {
                "[ ENTER ""1"" TO GO TO CHAPTER SELECTION ]",
                "[ ENTER ""2"" TO GO TO THE TAVERN ]",
                "[ ENTER ""0"" (ZERO) TO GO TO THE MAIN MENU ]"
            }

        Dim mForRndItem() As Integer = {7, 7, 7, 7, 7, 8, 7, 5, 7, 7, 9, 1, 7, 7, 7, 7, 7, 7, 5, 12, 7, 6, 4, 7, 7, 3, 7, 3, 3, 7, 3, 7, 3, 3, 7, 7, 7, 1, 10, 7, 7, 4, 7, 7, 2, 6, 3, 7, 7, 7, 11, 7, 7}

        Dim l, rndNum, num As Integer

        Dim pass As Boolean

        Dim str, userAns As String

        Check_tasks_for_complete()

        Console.SetCursorPosition(0, 2)
        Console.ForegroundColor = ConsoleColor.DarkYellow
        ImageOutputLineByLine(gameOver, 0.09, 25)
        Console.ResetColor()
        Console.SetCursorPosition(0, 10)
        'Линия между |GameOver| & |YouWon/YouLose|
        Console.SetCursorPosition(34, 9)
        '53
        For i = 1 To 53
            Console.Write("═")
            Pause(0.01)
        Next
        Console.SetCursorPosition(0, 11)
        If spNum = 0 Then
            Console.ForegroundColor = ConsoleColor.Cyan
            ImageOutputLineByLine(youWonPic, 0.07, 32)
            Console.ResetColor()
        Else
            Console.ForegroundColor = ConsoleColor.DarkRed
            ImageOutputLineByLine(youLosePic, 0.07, 30)
            Console.ResetColor()
        End If
        l = 0
        For i = 1 To 39
            Console.SetCursorPosition(0, l)
            Console.ForegroundColor = ConsoleColor.Yellow
            Console.Write("║║")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkCyan
            Console.Write("║║")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.Write("║║")
            Console.ResetColor()
            l += 1

            Pause(0.01)
        Next
        l = 0
        For i = 1 To 39
            Console.SetCursorPosition(114, l)
            Console.ForegroundColor = ConsoleColor.Yellow
            Console.Write("║║")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkCyan
            Console.Write("║║")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.Write("║║")
            Console.ResetColor()
            l += 1

            Pause(0.01)
        Next

        Console.ForegroundColor = ConsoleColor.Magenta

        DisplayingPictureFromSide(icon, 20, 20, 0.005)
        DisplayingPictureFromSide(m, 22, 29, 0.005)
        Console.ResetColor()

        Console.SetCursorPosition(48, 20)
        Console.ForegroundColor = ConsoleColor.DarkYellow
        Console.Write("[ YOU RECEIVED ]")
        Console.ResetColor()
        Pause(1)
        If spNum = 0 Then
            first_win_battle = True
            If enemyName = "knight" Then
                If Not done_first_season_tasks(7) Then
                    For j = 0 To UBound(defeat_3_crearures, 2)
                        If Not defeat_3_crearures(0, j) Then
                            defeat_3_crearures(0, j) = True
                            Exit For
                        End If
                    Next
                End If
            ElseIf enemyName = "mrSlime" Then
                If Not done_first_season_tasks(5) Then
                    For j = 0 To UBound(defeat_3_crearures, 2)
                        If Not defeat_3_crearures(1, j) Then
                            defeat_3_crearures(1, j) = True
                            Exit For
                        End If
                    Next
                End If
            ElseIf enemyName = "magicchest" Then
                If Not done_first_season_tasks(6) Then
                    For j = 0 To UBound(defeat_3_crearures, 2)
                        If Not defeat_3_crearures(2, j) Then
                            defeat_3_crearures(2, j) = True
                            Exit For
                        End If
                    Next
                End If
            End If
            Select Case chapter_num
                Case 1
                    'Глава 1
                    If levelNum = 4 Then

                    Else
                        doneLevelsFromCharpet1(levelNum - 1) = True
                    End If
                Case 2
                    'Глава 2
                    If levelNum = 4 Then

                    Else
                        doneLevelsFromCharpet2(levelNum - 1) = True
                    End If
                Case 3
                    'Глава 3
                    If levelNum = 4 Then

                    Else
                        doneLevelsFromCharpet3(levelNum - 1) = True
                    End If
            End Select
            Console.SetCursorPosition(48, 22)
            Console.ForegroundColor = ConsoleColor.DarkCyan
            Console.Write("[ COINS ] ")
            Pause(1)
            Console.ResetColor()
            Randomize()
            Do
                rndNum = Rnd() * (300 - 70) + 70
                If rndNum Mod 5 = 0 Then
                    Exit Do
                End If
            Loop

            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write(rndNum)
            playerCoins += rndNum
            Console.ResetColor()
            Pause(1)
            Console.SetCursorPosition(48, 24)
            Console.ForegroundColor = ConsoleColor.DarkCyan
            Console.Write("[ RANDOM ITEM ] ")
            Pause(2)
            Console.ResetColor()
            rndNum = Rnd() * (UBound(mForRndItem) - 0) + 0
            num = mForRndItem(rndNum)
            For i = 0 To UBound(playerInventory)
                If playerInventory(i) Then
                    pass = False
                Else
                    pass = True
                End If
            Next
            Console.ForegroundColor = ConsoleColor.Cyan
            Select Case num
                Case 1
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "BLUE SCARF"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("BLUE SCARF")
                        maxPlayerXp += 20
                    Else
                        Console.Write("BLUE SCARF = [ 120 COINS ]")
                        playerCoins += 120
                    End If
                Case 2
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "CHOCOLATE MARIO MUSHROOM"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("CHOCOLATE MARIO MUSHROOM")
                    Else
                        Console.Write("CHOCOLATE MARIO MUSHROOM = [ 250 COINS ]")
                        playerCoins += 250
                    End If
                Case 3
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "GOLDEN APPLE"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("GOLDEN APPLE")
                    Else
                        Console.Write("GOLDEN APPLE = [ 100 COINS ]")
                        playerCoins += 100
                    End If
                Case 4
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "CHAIN MAIL"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("CHAIN MAIL")
                        maxPlayerXp += 25
                    Else
                        Console.Write("CHAIN MAIL = [ 75 COINS ]")
                        playerCoins += 75
                    End If
                Case 5
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "LAMB SWEATER"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("LAMB SWEATER")
                        maxPlayerXp += 15
                    Else
                        Console.Write("LAMB SWEATER = [ 150 COINS ]")
                        playerCoins += 150
                    End If
                Case 6
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "GREEN SHORTS"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("GREEN SHORTS")
                        maxPlayerXp += 5
                    Else
                        Console.Write("GREEN SHORTS = [ 60 COINS ]")
                        playerCoins += 60
                    End If
                Case 7
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "GASING [BLUE LAGOON]"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("GASING [BLUE LAGOON]")
                    Else
                        Console.Write("GASING [BLUE LAGOON] = [ 20 COINS ]")
                        playerCoins += 20
                    End If
                Case 8
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "WOODEN SWORD"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("WOODEN SWORD")
                        maxPlayerXp += 5
                    Else
                        Console.Write("WOODEN SWORD = [ 110 COINS ]")
                        playerCoins += 110
                    End If
                Case 9
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "BINOCULARS"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("BINOCULARS")
                        maxPlayerXp += 10
                    Else
                        Console.Write("BINOCULARS = [ 80 COINS ]")
                        playerCoins += 80
                    End If
                Case 10
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "IRON SWORD"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("IRON SWORD")
                        maxPlayerXp += 10
                    Else
                        Console.Write("IRON SWORD = [ 170 COINS ]")
                        playerCoins += 170
                    End If
                Case 11
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "DRUID'S STAFF"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("DRUID'S STAFF")
                        maxPlayerXp += 50
                    Else
                        Console.Write("DRUID'S STAFF = [ 400 COINS ]")
                        playerCoins += 400
                    End If
                Case 12
                    If pass Then
                        For i = 0 To UBound(playerInventory)
                            If Not playerInventory(i) Then
                                playerInventory(i) = True
                                playerInventoryNames(i) = "BOOK OF THE DAMNED"
                                Exit For
                            End If
                        Next
                        playerBag += 1
                        Console.Write("BOOK OF THE DAMNED")
                        maxPlayerXp += 75
                    Else
                        Console.Write("BOOK OF THE DAMNED = [ 2500 COINS ]")
                        playerCoins += 2500
                    End If
            End Select
            Console.ResetColor()
        Else
            Console.SetCursorPosition(48, 22)
            Console.ForegroundColor = ConsoleColor.DarkCyan
            Console.Write("[ COINS ] ")
            Pause(1)
            Console.ResetColor()
            Randomize()
            Do
                rndNum = Rnd() * (300 - 70) + 70
                If rndNum Mod 5 = 0 Then
                    Exit Do
                End If
            Loop

            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write(rndNum)
            playerCoins += rndNum
            Console.ResetColor()
            Pause(1)
            Console.SetCursorPosition(48, 24)
            Console.ForegroundColor = ConsoleColor.DarkCyan
            Console.Write("[ RANDOM ITEM ] ")
            Pause(2)
            Console.Write("NULL")
        End If

        Pause(1.5)

        Select Case chapter_num
            Case 1
                If levelNum = 4 Then
                    If spNum = 0 Then
                        If doneLevelsFromCharpet1(3) = False Then
                            doneLevelsFromCharpet1(3) = False
                            Congrats_posters(chapter_num)
                        End If
                    End If
                End If
            Case 2
                If levelNum = 4 Then
                    If spNum = 0 Then
                        If doneLevelsFromCharpet2(3) = False Then
                            doneLevelsFromCharpet2(3) = True
                            Congrats_posters(chapter_num)
                        End If
                    End If
                End If
            Case 3
                If levelNum = 4 Then
                    If spNum = 0 Then
                        If doneLevelsFromCharpet3(3) = False Then
                            doneLevelsFromCharpet3(3) = True
                            Congrats_posters(chapter_num)
                        End If
                    End If
                End If
        End Select

        'Вывод информации
        l = 30
        For i = 0 To UBound(infoTable)
            str = infoTable(i)
            LineOutPutCharacterByCharacter(str, 48, l, 0.04, ConsoleColor.Yellow)

            l += 2
        Next

        Check_tasks_for_complete()

        Console.SetCursorPosition(48, l + 2)
        Console.Write("-> ")
        Console.ForegroundColor = ConsoleColor.Cyan
        userAns = Console.ReadLine()
        Console.ResetColor()

        result = CheckUserAns(userAns, 0, 2)

        If result Then
            Select Case userAns
                Case "1"
                    DungeonMenuPath()
                Case "2"
                    Tavern()
                Case "0"
                    mainMenu()
            End Select
        Else
            mainMenu()
        End If

        Console.ReadKey()
    End Sub
    Sub LevelMainPlaceForInfo(levelNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0015

        Dim level1Pic() As String =
            {
                "╔╗──╔═══╗╔╗╔╗╔═══╗╔╗────╔╗",
                "║║──║╔══╝║║║║║╔══╝║║───╔╝║",
                "║║──║╚══╗║║║║║╚══╗║║───╚╗║",
                "║║──║╔══╝║╚╝║║╔══╝║║────║║",
                "║╚═╗║╚══╗╚╗╔╝║╚══╗║╚═╗──║║",
                "╚══╝╚═══╝─╚╝─╚═══╝╚══╝──╚╝"
            }

        Dim level2Pic() As String =
            {
                "╔╗──╔═══╗╔╗╔╗╔═══╗╔╗──╔══╗",
                "║║──║╔══╝║║║║║╔══╝║║──╚═╗║",
                "║║──║╚══╗║║║║║╚══╗║║──╔═╝║",
                "║║──║╔══╝║╚╝║║╔══╝║║──║╔═╝",
                "║╚═╗║╚══╗╚╗╔╝║╚══╗║╚═╗║╚═╗",
                "╚══╝╚═══╝─╚╝─╚═══╝╚══╝╚══╝"
            }

        Dim level3Pic() As String =
            {
                "╔╗──╔═══╗╔╗╔╗╔═══╗╔╗──╔══╗",
                "║║──║╔══╝║║║║║╔══╝║║──╚═╗║",
                "║║──║╚══╗║║║║║╚══╗║║──╔═╝║",
                "║║──║╔══╝║╚╝║║╔══╝║║──╚═╗║",
                "║╚═╗║╚══╗╚╗╔╝║╚══╗║╚═╗╔═╝║",
                "╚══╝╚═══╝─╚╝─╚═══╝╚══╝╚══╝"
            }

        Dim level4Pic() As String =
            {
                "╔╗──╔═══╗╔╗╔╗╔═══╗╔╗──╔╗╔╗",
                "║║──║╔══╝║║║║║╔══╝║║──║║║║",
                "║║──║╚══╗║║║║║╚══╗║║──║╚╝║",
                "║║──║╔══╝║╚╝║║╔══╝║║──╚═╗║",
                "║╚═╗║╚══╗╚╗╔╝║╚══╗║╚═╗──║║",
                "╚══╝╚═══╝─╚╝─╚═══╝╚══╝──╚╝"
            }

        Dim infoPic() As String =
            {
                "╔══╗ ╔╗─╔╗ ╔══╗ ╔══╗",
                "╚╗╔╝ ║╚═╝║ ║╔═╝ ║╔╗║",
                "─║║─ ║╔╗ ║ ║╚═╗ ║║║║",
                "─║║─ ║║╚╗║ ║╔═╝ ║║║║",
                "╔╝╚╗ ║║ ║║ ║║   ║╚╝║",
                "╚══╝ ╚╝ ╚╝ ╚╝   ╚══╝"
            }

        Dim roomPic() As String =
            {
                "╔═══╗ ╔══╗ ╔══╗ ╔╗  ╔╗",
                "║╔═╗║ ║╔╗║ ║╔╗║ ║║  ║║",
                "║╚═╝║ ║║║║ ║║║║ ║╚╗╔╝║",
                "║╔╗╔╝ ║║║║ ║║║║ ║╔╗╔╗║",
                "║║║║  ║╚╝║ ║╚╝║ ║║╚╝║║",
                "╚╝╚╝  ╚══╝ ╚══╝ ╚╝  ╚╝"
            }

        Dim l As Integer

        l = 2

        'Вывод полос слева
        For i = 1 To 37
            Console.SetCursorPosition(1, l)
            Console.ForegroundColor = ConsoleColor.DarkCyan
            Console.Write("║║")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Yellow
            Console.Write("║║")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.Write("║║")
            Console.ResetColor()
            l += 1

            Pause(0.01)
        Next

        l = 2

        'Вывод полос справа
        For i = 1 To 37
            Console.SetCursorPosition(134, l)
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.Write("║║")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Yellow
            Console.Write("║║")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkCyan
            Console.Write("║║")
            Console.ResetColor()
            l += 1

            Pause(0.01)
        Next

        'Вывод полос сверху донизу
        Console.ForegroundColor = ConsoleColor.DarkCyan
        For i = 2 To 38
            Console.SetCursorPosition(55, i)
            Console.Write("║║                      ║║")

            Pause(0.01)
        Next
        Console.ResetColor()

        'Вывод номера уровня
        Console.ForegroundColor = ConsoleColor.Cyan
        Select Case levelNum
            Case 1
                DisplayingPictureFromSide(level1Pic, 55, 17, speed)
            Case 2
                DisplayingPictureFromSide(level2Pic, 55, 17, speed)
            Case 3
                DisplayingPictureFromSide(level3Pic, 55, 17, speed)
            Case 4
                DisplayingPictureFromSide(level4Pic, 55, 17, speed)
        End Select
        Console.ResetColor()

        'Вывод надписи |info|
        Console.ForegroundColor = ConsoleColor.Magenta
        DisplayingPictureFromSide(infoPic, 20, 2, speed)
        'Вывод надписи |room|
        DisplayingPictureFromSide(roomPic, 98, 2, speed)
        Console.ResetColor()
    End Sub
    Sub Level(levelNum As Integer, chapter_num As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim attentionInfo() As String =
            {
                "[ AFTER THE START OF LEVEL GENERATION    |",
                "| - LEAVING TO THE END OF THE LEVEL      |",
                "| LEADS TO A PENALTY IN THE SIZE OF 20 C |",
                "| IN THE ABSENCE OF COINS,               |",
                "| STAMIN POINTS WILL BE DECLARED         ]",
                "[ TO START LEVEL - ENTER ""START"" ]",
                "[ ENTER ""0"" (ZERO) TO RETURN ]"
            }

        Dim m() As String =
            {
                "",
                "",
                "",
                "",
                "",
                ""
            }

        Dim icon(7) As String

        Dim standartMode() As String =
            {
                "╬════════════════════════════════════╬",
                "║╔═══╦════╦═══╦═╗│╔╦═══╦═══╦═══╦════╗║",
                "║║╔═╗║╔╗╔╗║╔═╗║║╚╗║╠╗╔╗║╔═╗║╔═╗║╔╗╔╗║║",
                "║║╚══╬╝║║╚╣║│║║╔╗╚╝║║║║║║│║║╚═╝╠╝║║╚╝║",
                "║╚══╗║│║║│║╚═╝║║╚╗║║║║║║╚═╝║╔╗╔╝│║║││║",
                "║║╚═╝║│║║│║╔═╗║║│║║╠╝╚╝║╔═╗║║║╚╗│║║││║",
                "║╚═══╝│╚╝│╚╝│╚╩╝│╚═╩═══╩╝│╚╩╝╚═╝│╚╝││║",
                "╬════════════════════════════════════╬"
            }

        Dim cardGame() As String =
            {
                "╬═══════════════════════════════════╬",
                "║╔═══╦═══╦═══╦═══╗╔═══╦═══╦═╗╔═╦═══╗║",
                "║║╔═╗║╔═╗║╔═╗╠╗╔╗║║╔═╗║╔═╗║│╚╝│║╔══╝║",
                "║║║│╚╣║│║║╚═╝║║║║║║║│╚╣║│║║╔╗╔╗║╚══╗║",
                "║║║│╔╣╚═╝║╔╗╔╝║║║║║║╔═╣╚═╝║║║║║║╔══╝║",
                "║║╚═╝║╔═╗║║║╚╦╝╚╝║║╚╩═║╔═╗║║║║║║╚══╗║",
                "║╚═══╩╝│╚╩╝╚═╩═══╝╚═══╩╝│╚╩╝╚╝╚╩═══╝║",
                "╬═══════════════════════════════════╬"
            }

        Dim infoItems() As String =
            {
                "[ ENTER ""START"" TO START GAME ]",
                "[ ENTER ""0""(ZERO) TO LEAVE GAME SETTINGS ]",
                "[ EXIT PENALTY: 20 C ]"
            }

        Dim l As Integer
        Dim str, userAns, gameMode, enemy, name As String
        Dim symb As Char

        enemy = ""
        name = ""

        'Выбор режима игры
        gameMode = GameModes()
        'Выбор противника
        enemysFromCharpet1(m, enemy, name)
        'Выбор иконки противника
        icon = IssuingAnIcon(name)

        Do
            LevelMainPlaceForInfo(levelNum)

            'Вывод информации
            l = 14
            For i = 0 To UBound(attentionInfo)
                str = attentionInfo(i)
                LineOutPutCharacterByCharacter(str, 10, l, speed + 0.0017, ConsoleColor.Yellow)

                If i = 4 Then
                    l += 4
                ElseIf i = 5 Then
                    l += 4
                Else
                    l += 2
                End If
            Next

            Console.SetCursorPosition(100, 32)
            str = "═════════════════"
            For i = 0 To str.Length - 1
                symb = str(i)
                Console.Write(symb)
                Pause(0.04)
            Next

            Console.SetCursorPosition(100, 30)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            userAns = userAns.ToUpper

            Select Case userAns
                Case "0"
                    'Возвращение в выбор уровня
                    Select Case chapter_num
                        Case 1
                            mapFirstChapter()
                        Case 2
                            mapSecondChapter()
                        Case 3
                            mapFirdChapter()
                    End Select
                Case "START"
                    'Начало генерации уровня
                    Exit Do
                Case Else
                    str = "[ INVALID VALUE ]"
                    LineOutPutCharacterByCharacter(str, 100, 30, speed + 0.08, ConsoleColor.Red)
                    speed = 0
                    Pause(2)
                    Console.Clear()
            End Select
        Loop

        Do
            LevelMainPlaceForInfo(levelNum)

            'Вывод информации
            'Вывод иконки противника
            Console.ForegroundColor = ConsoleColor.Yellow
            DisplayingPictureFromSide(icon, 14, 14, speed)
            Console.ResetColor()
            'Вывод изображения соперника
            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(m, 34, 16, speed)
            Console.ResetColor()
            'Вывод |YOUR ENEMY|
            Console.SetCursorPosition(18, 11)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.Write("[ Y O U R  E N E M Y ]")
            Console.ResetColor()
            'Вывод имени противника
            Console.SetCursorPosition(31, 14)
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write(enemy)
            Console.ResetColor()
            'Вывод |GAME MODE|
            Console.SetCursorPosition(20, 26)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.Write("[ G A M E  M O D E ]")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Yellow
            Select Case gameMode
                Case "STANDART"
                    DisplayingPictureFromSide(standartMode, 12, 29, speed)
                Case "CARD GAME"
                    DisplayingPictureFromSide(cardGame, 12, 29, speed)
            End Select
            Console.ResetColor()
            'Вывод |inventiry|
            Console.SetCursorPosition(94, 11)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.Write("[ Y O U R  I N V E N T O R Y ]")
            For i = 0 To UBound(playerInventory)
                If Not playerInventory(i) Then
                    playerInventoryNames(i) = "NULL"
                End If
            Next
            Console.ResetColor()
            l = 16
            Console.ForegroundColor = ConsoleColor.DarkCyan
            For i = 0 To UBound(playerInventory)
                Console.SetCursorPosition(94, l)
                Console.Write("{0}| ", i + 1)
                Console.Write(playerInventoryNames(i))
                l += 2
            Next
            Console.ResetColor()
            'Вывод информации
            l = 33
            For i = 0 To UBound(infoItems)
                str = infoItems(i)
                LineOutPutCharacterByCharacter(str, 86, l, speed, ConsoleColor.Yellow)
                l += 2
            Next

            Console.SetCursorPosition(94, 29)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            userAns = userAns.ToUpper

            Select Case userAns
                Case "0"
                    If playerCoins >= 20 Then
                        playerCoins -= 20
                    Else
                        playerCoins = 0
                    End If
                    Select Case chapter_num
                        Case 1
                            mapFirstChapter()
                        Case 2
                            mapSecondChapter()
                        Case 3
                            mapFirdChapter()
                    End Select
                Case "START"
                    'Старт уровня
                    Select Case gameMode
                        Case "STANDART"
                            StandartGameMode(m, name, icon, enemy, chapter_num, levelNum)
                        Case "CARD GAME"
                            startCardGame(m, name, icon, enemy, chapter_num, levelNum)
                    End Select
                Case "start"
                    'Старт уровня
                    Select Case gameMode
                        Case "STANDART"
                            StandartGameMode(m, name, icon, enemy, chapter_num, levelNum)
                        Case "CARD GAME"
                            startCardGame(m, name, icon, enemy, chapter_num, levelNum)
                    End Select
                Case Else
                    str = "[ INVALID VALUE ]"
                    LineOutPutCharacterByCharacter(str, 94, 29, 0.004, ConsoleColor.Red)
                    speed = 0
                    Pause(2)
                    Console.Clear()
            End Select
        Loop

    End Sub
    Sub StartCardGame(m, name, icon, enemy, chapter_num, levelNum)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim card_hand_pic() As String = {
           "         ▄▄▄▄▄▄▄▄████▄▄▄▄             ",
           "      ▄▄█▀▀▀▀     █    ▀▀▀▀██▄▄       ",
           "   ▄█▀▀ ██       █▀        ▄███       ",
           "▄█▀▀    ██     █▄█     ▄ ▄█▀  ▀█▄     ",
           " ▀█    ███   ▄███▀  ▄████▀      ▀█▄   ",
           "  ▀█    ██  ▀████  ▀███▀ ▄▄       █▄  ",
           "   ▀█▄ ████    █▀▄▄█▀▀  ████▄      ▀█▄",
           "     █▄ ▀▀█   ▄████▄    ██████   ▄█▀▀ ",
           "      █▄  █   ██▀▀█     ▀▀▀▀▀▀ ▄█▀    ",
           "       ▀█ ██▄▄█  ██▄         ▄█▀      ",
           "        ▀█▄██▀  ▄█▄██▄     ▄█▀        ",
           "         █▀     █ ▀  ██▄▄▄█▀          ",
           "         █      █   ▀  █▀             ",
           "         █      █    ▄█▀              ",
           "         █▄     ▀    █                ",
           "         ▄█         █▀                ",
           "        ███▄▄▄     ▄▀                 ",
           "       ▀████████▄▄█▀                  ",
           "          ▀▀▀█████▄                   "
                               }

        Dim black_str_pic() As String =
            {
                "╔══╗─╔╗──╔══╗╔══╗╔╗╔══╗",
                "║╔╗║─║║──║╔╗║║╔═╝║║║╔═╝",
                "║╚╝╚╗║║──║╚╝║║║──║╚╝║  ",
                "║╔═╗║║║──║╔╗║║║──║╔╗║  ",
                "║╚═╝║║╚═╗║║║║║╚═╗║║║╚═╗",
                "╚═══╝╚══╝╚╝╚╝╚══╝╚╝╚══╝"
            }

        Dim jack_str_pic() As String =
            {
                "─╔══╗╔══╗╔══╗╔╗╔══╗",
                "─╚╗╔╝║╔╗║║╔═╝║║║╔═╝",
                "──║║─║╚╝║║║──║╚╝║  ",
                "╔╗║║─║╔╗║║║──║╔╗║  ",
                "║╚╝╚╗║║║║║╚═╗║║║╚═╗",
                "╚═══╝╚╝╚╝╚══╝╚╝╚══╝"
            }

        'Вывод изображения руки
        Console.SetCursorPosition(0, 10)
        ImageOutputLineByLine(card_hand_pic, 0.017, 55)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.DarkMagenta
        'Вывод |black|
        DisplayingPictureFromSide(black_str_pic, 34, 18, speed)
        'Вывод |jack|
        Console.BackgroundColor = ConsoleColor.White
        Console.ForegroundColor = ConsoleColor.Black
        DisplayingPictureFromSide(jack_str_pic, 92, 18, speed)
        Console.BackgroundColor = ConsoleColor.Black
        Console.ResetColor()

        'Вывод линиии под изображением и над
        For i = 0 To 75
            Console.SetCursorPosition(35 + i, 8)
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.Write("─")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.SetCursorPosition(35 + i, 7)
            Console.Write("─")
            Console.ResetColor()
            Pause(0.0035)
        Next
        For i = 0 To 75
            Console.SetCursorPosition(35 + i, 30)
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.Write("─")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.SetCursorPosition(35 + i, 31)
            Console.Write("─")
            Console.ResetColor()
            Pause(0.0035)
        Next

        Pause(3)
        'Переход в основной режим игры
        black_jack_main(name, icon, m, chapter_num, levelNum)
    End Sub
    'Рисование стен и пространства между ними
    Sub DrawWalls(color1Col As ConsoleColor, color2Col As ConsoleColor, colorBackGround As ConsoleColor, height As Integer)
        'Рисуем стену |1|
        Console.SetCursorPosition(100, 0)

        For i = 0 To height
            Console.ForegroundColor = color1Col
            Console.Write("██")
            Console.ResetColor()
            Console.ForegroundColor = color2Col
            Console.Write("██" & Chr(10))
            Console.ResetColor()
            Console.SetCursorPosition(100, i)
        Next
        'Рисуем стену |2|
        Console.SetCursorPosition(156, 0)

        For i = 0 To height
            Console.ForegroundColor = color1Col
            Console.Write("██")
            Console.ResetColor()
            Console.ForegroundColor = color2Col
            Console.Write("██" & Chr(10))
            Console.ResetColor()
            Console.SetCursorPosition(156, i)
        Next

        'Рисуем пространство между стенами
        Console.SetCursorPosition(104, 0)

        Console.BackgroundColor = colorBackGround
        For i = 0 To height - 1
            Console.SetCursorPosition(104, i)
            For j = 1 To 26
                Console.Write("  ")
            Next
        Next
        Console.BackgroundColor = ConsoleColor.Black
        Console.ResetColor()
    End Sub
    'Отрисовывание стен, |cards|, публики | Игровой процесс
    Sub DrawMainPartOfGamePlace(str1() As String, str3() As String, str4() As String)
        Dim rndNum, p As Integer
        'Отрисовываем стены
        DrawWalls(ConsoleColor.DarkMagenta, ConsoleColor.DarkMagenta, ConsoleColor.Gray, 48)
        'Отрисовываем |Dealer|
        Console.ForegroundColor = ConsoleColor.Cyan
        DisplayingPictureFromSide(str1, 106, 2, speed - 0.0053)
        Console.ResetColor()
        'Выводим ограничение смотрящих мест
        Console.ForegroundColor = ConsoleColor.Gray
        Console.SetCursorPosition(0, 10)
        Console.Write(StrDup(100, "="))
        Console.SetCursorPosition(0, 11)
        Console.Write(StrDup(100, "|"))
        Console.SetCursorPosition(0, 12)
        Console.Write(StrDup(100, "|"))
        Console.ResetColor()
        'Выводим публику
        For i = 1 To 7
            Randomize()
            rndNum = Int((2 * Rnd()) + 1)

            If i = 7 Then
                p = 0
            Else
                p += 14
            End If

            If rndNum = 1 Then
                Console.SetCursorPosition(p, 2)
                For j = 0 To UBound(str3)
                    Console.WriteLine(str3(j))
                    Console.SetCursorPosition(p, j + 1 + 2)
                Next
            Else
                Console.SetCursorPosition(p, 0)
                For j = 0 To UBound(str4)
                    Console.WriteLine(str4(j))
                    Console.SetCursorPosition(p, j + 1)
                Next
            End If
        Next
    End Sub

    Sub Black_jack_main(enemyName As String, icon() As String, m() As String, chapter_num As Integer, levelNum As Integer)
        Console.SetBufferSize(160, 50)
        Console.SetWindowSize(160, 50)
        Console.Clear()

        Dim str1() As String = {
            "██████╗ ███████╗ █████╗ ██╗     ███████╗██████╗ ",
            "██╔══██╗██╔════╝██╔══██╗██║     ██╔════╝██╔══██╗",
            "██║  ██║█████╗  ███████║██║     █████╗  ██████╔╝",
            "██║  ██║██╔══╝  ██╔══██║██║     ██╔══╝  ██╔══██╗",
            "██████╔╝███████╗██║  ██║███████╗███████╗██║  ██║",
            "╚═════╝ ╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝"
                               }

        Dim str2() As String = {
            "░       ░░      ░░       ░",
            "░░░     ░░      ░░     ░░░",
            "░░░░   ░░░░    ░░░░   ░░░░",
            "░░░░░░░░░░░░  ░░░░░░░░░░░░",
            " ░░░░░░░░░░░  ░░░░░░░░░░░ ",
            "  ░░░░░░░░░    ░░░░░░░░░  ",
            "            ░░            ",
            "   ░░░      ░░     ░░░    ",
            "                          ",
            " ░░░░░░░░░░░  ░░░░░░░░░░░ ",
            "░░░      ░░░░░░░░      ░░░",
            "░░░        ░░░░        ░░░",
            "   ░░   ░░░░  ░░░░   ░░   ",
            "     ░░░░░░    ░░░░░░     ",
            "      ░░░░░    ░░░░░      ",
            "       ░░░░░░░░░░░░       ",
            "       ░░ ░░  ░░ ░░       ",
            "          ░░  ░░          "
                               }

        Dim str3() As String = {
            "   ______     ",
            "  ///\\\\\    ",
            " //    \\\\   ",
            "  | -  - |\\  ",
            "  |  j   |\\\ ",
            "   | __  |    ",
            "   |     |    ",
            "|__|     |__| "
                             }

        Dim str4() As String = {
            "   ______     ",
            "  ///\\\\\    ",
            " //    \\\\   ",
            "  | -  - |\\  ",
            "  |  j   |\\\ ",
            "   | __  |    ",
            "   |     |    ",
            "   |     |    ",
            "   |     |    ",
            "|__|     |__| "
                             }

        Dim h, c, f, l As Integer 'Служебные переменные

        Dim amountOfPart, sum, i1, j1, amountOfBots, blackJack_coins, num As Integer

        Dim str As String

        Dim userAns, specialNameFromList As String

        Dim result, go As Boolean

        amountOfPart = 3

        Dim playersCards(2, amountOfPart) As Integer

        Dim gameBets(amountOfPart - 1) As Integer

        Dim playersVariants(amountOfPart - 1) As Boolean

        Dim denomination(2, amountOfPart) As Integer

        Dim sumOfCardsOfPlayers(amountOfPart) As Integer

        Dim winners(amountOfPart - 1) As Boolean

        Dim helpM() As String

        amountOfBots = 3

        Dim botsNames(amountOfBots - 1) As String

        Dim amountOfAllGames, rndNum As Byte

        Dim frame_pic() As String =
        {
            "═════════════════════════════════════════════════════════════════════║",
            "║                                                                    ║",
            "║                                                                    ║",
            "║                                                                    ║",
            "║                                                                    ║",
            "║                                                                    ║",
            "║                                                                    ║",
            "║                                                                    ║",
            "║                                                                    ║",
            "║═════════════════════════════════════════════════════════════════════"
        }

        Dim namesForBots() As String = {
            "Adam", "Alan", "Alex", "Albert", "Andrea", "Benjamin", "Bill", "Bob", "Bobby", "Brian", "Brandon", "Bruce", "Adriana", "Albina", "Angelina", "Angela", "Angelica", "Ariel", "Barbara", "Bella", "Betty", "Brenda", "Bianca", "Vanessa", "Vladimir", "Gennady", "Victoria", "Maria"
                                       }

        speed = 0.004

        blackJack_coins = 100

        'Кол-во игр
        Randomize()
        amountOfAllGames = Rnd() * (7 - 3) + 3

        'Выдача имён ботам для BlackJack
        rndNum = 1 + ((UBound(namesForBots) - 1 + 0) * Rnd())
        specialNameFromList = namesForBots(rndNum)
        botsNames(0) = UCase(specialNameFromList)
        For i = 1 To UBound(botsNames)
            Do
                Randomize()
                rndNum = 1 + ((UBound(namesForBots) - 1 + 0) * Rnd())
                specialNameFromList = namesForBots(rndNum)

                For j = 0 To UBound(botsNames)
                    If specialNameFromList = botsNames(0)(j) Then
                        result = False
                        Exit For
                    Else
                        result = True
                        botsNames(i) = UCase(specialNameFromList)
                    End If
                Next
            Loop Until result
        Next

        Do
            DrawMainPartOfGamePlace(str1, str3, str4)
            'Череп
            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(str2, 37, 5, speed - 0.006)
            Console.ResetColor()
            'Рамка
            Console.SetCursorPosition(0, 28)
            Console.ForegroundColor = ConsoleColor.Cyan
            ImageOutputLineByLine(frame_pic, speed + 0.06, 15)
            Console.ResetColor()

            'Выводим текст
            Select Case h
                Case 0
                    str = "LADIES AND GENTLEMEN, WELCOME TO ANOTHER BLACK JACK GAME!"
                    LineOutPutCharacterByCharacter(str, 18, 30, 0.015, ConsoleColor.White)
                    str = "[ PRESS ANY KEY TO CONTINUE ]"
                    LineOutPutCharacterByCharacter(str, 18, 36, 0.015, ConsoleColor.Yellow)
                Case 1
                    str = ""
                    str &= "YOUR FAVOURITE CROUPIER WILSON IS WITH YOU AS USUAL !"
                    LineOutPutCharacterByCharacter(str, 18, 30, 0.015, ConsoleColor.White)
                    str = "[ PRESS ANY KEY TO CONTINUE ]"
                    LineOutPutCharacterByCharacter(str, 18, 36, 0.015, ConsoleColor.Yellow)
                Case 2
                    str = ""
                    str &= "WE HAVE THREE PARTICIPANTS AT THE TABLE TODAY:"
                    LineOutPutCharacterByCharacter(str, 18, 30, 0.015, ConsoleColor.White)
                    Console.SetCursorPosition(18, 32)
                    l = 32
                    For i = 0 To 1
                        Console.SetCursorPosition(18, l)
                        Console.Write("-> ")
                        LineOutPutCharacterByCharacter(botsNames(i), 21, l, 0.045, ConsoleColor.DarkYellow)
                        l += 1
                        Pause(1)
                    Next
                    Console.SetCursorPosition(18, l)
                    Console.Write("-> ")
                    LineOutPutCharacterByCharacter("PIP", 21, l, 0.045, ConsoleColor.DarkYellow)
                    Pause(1)
                    Console.SetCursorPosition(18, 36)
                    Console.ForegroundColor = ConsoleColor.Yellow
                    str = "[ PRESS ANY KEY TO CONTINUE ]"
                    LineOutPutCharacterByCharacter(str, 18, 36, 0.015, ConsoleColor.Yellow)
                    Console.ResetColor()
                Case 3
                    str = ""
                    str &= "IS BEING PLAYED TODAY " & amountOfAllGames & " GAMES." & " | WHICH MEANS IT WILL BE FUNNY! "
                    LineOutPutCharacterByCharacter(str, 18, 30, 0.04, ConsoleColor.White)
                    Pause(1)
                    str = ""
                    str &= "I REMIND YOU THAT WE ARE A RESPECTABLE ESTABLISHMENT, "
                    LineOutPutCharacterByCharacter(str, 18, 32, 0.04, ConsoleColor.White)
                    str = ""
                    str &= "SO WE PLAY FOR BISCUITS [B - BISCUITS]"
                    LineOutPutCharacterByCharacter(str, 18, 34, 0.04, ConsoleColor.White)
                    Console.ForegroundColor = ConsoleColor.Yellow
                    str = "[ PRESS ANY KEY TO CONTINUE ]"
                    LineOutPutCharacterByCharacter(str, 18, 36, 0.015, ConsoleColor.Yellow)
                Case 4
                    str = ""
                    str &= "OUR CLUB SMELLS OF TEA, WHICH MEANS IT'S GAME TIME!"
                    LineOutPutCharacterByCharacter(str, 18, 30, 0.04, ConsoleColor.White)
                    Console.SetCursorPosition(18, 36)
                    str = "[ PRESS ANY KEY TO CONTINUE ]"
                    LineOutPutCharacterByCharacter(str, 18, 36, 0.015, ConsoleColor.Yellow)
                    Console.ResetColor()
            End Select

            Console.ResetColor()
            Console.ReadKey()
            Console.Clear()
            speed = 0.0001

            h += 1
        Loop Until h = 5
        f = 0
        Do
            c = 1
            h = 0
            For i = 0 To UBound(playersCards, 1)
                For j = 0 To UBound(playersCards, 2)
                    playersCards(i, j) = 0
                    denomination(i, j) = 0
                Next
            Next
            Do Until go
                h += 1
                c += 1
                Console.Clear()
                Console.BackgroundColor = ConsoleColor.Black
                go = False
                'Отрисовывание игрового поля
                DrawMainPartOfGamePlace(str1, str3, str4)
                'Вывод ставок Ботов | Игрока
                Console.BackgroundColor = ConsoleColor.White
                Console.ForegroundColor = ConsoleColor.Black
                Console.SetCursorPosition(106, 10)
                Console.BackgroundColor = ConsoleColor.Gray
                Console.ForegroundColor = ConsoleColor.Magenta
                Console.WriteLine("| PLAYERS' BETS |")
                Console.ResetColor()
                Console.ForegroundColor = ConsoleColor.Black
                Console.BackgroundColor = ConsoleColor.Gray

                For i = 1 To amountOfPart
                    Console.SetCursorPosition(106, 10 + 2 * i)
                    If i = 1 Or i = 2 Then
                        If h = 1 Then
                            'Боты вводят ставки
                            gameBets(i - 1) = BetsOfBots()
                        End If
                        Console.Write(botsNames(i) & ": ")
                        Console.Write(gameBets(i - 1))
                    Else
                        If h = 1 Then
                            If f = amountOfAllGames - 1 Then
                                Console.Write("{0}, [ LAST ROUND | RATE = {1} ]", userName, blackJack_coins)
                                userAns = blackJack_coins.ToString
                                result = True
                            Else
                                Console.Write("{0}, ENTER THE BET [{1}-{2}] -> ", userName, blackJack_coins \ 2, blackJack_coins)
                                userAns = Console.ReadLine()
                                result = CheckUserAns(userAns, blackJack_coins \ 2, blackJack_coins)
                            End If

                            If result Then
                                num = userAns
                                gameBets(i - 1) = num
                                Console.SetCursorPosition(106, 10 + 2 * i)
                                Console.Write(StrDup(45, " "))
                                Console.SetCursorPosition(106, 10 + 2 * i)
                                Console.Write("{0}: {1}", userName, gameBets(i - 1))
                                go = True 'ПРОХОЖДЕНИЕ ДАЛЬШЕ
                                h = 1
                            Else
                                Console.BackgroundColor = ConsoleColor.Gray
                                Console.ForegroundColor = ConsoleColor.DarkRed
                                str = "[ INVALID VALUE ENTERED ]"
                                LineOutPutCharacterByCharacter(str, 106, 18, 0.04, ConsoleColor.DarkRed)
                                Pause(1)
                                c = 1
                                h = 0
                                go = False
                                Exit For
                            End If
                        Else
                            Console.Write(userName & ": ")
                            Console.Write(gameBets(i - 1))
                        End If
                    End If
                Next

                If h >= 2 Then
                    go = True
                End If
                'Сумма печенек у игрока в разделе |dealer|
                Console.SetCursorPosition(126, 10)
                Console.ForegroundColor = ConsoleColor.Magenta
                Console.BackgroundColor = ConsoleColor.Gray
                Console.WriteLine("| PLAYER BISCUITS: " & blackJack_coins & " |")
                Console.BackgroundColor = ConsoleColor.Black
                Console.ResetColor()
                Console.SetCursorPosition(14, 14)
                For j = 13 To 47
                    Console.SetCursorPosition(17, j)
                    Console.BackgroundColor = ConsoleColor.White
                    Console.Write(" ")
                    Console.SetCursorPosition(18, j)
                    Console.BackgroundColor = ConsoleColor.DarkGreen
                    Console.Write(StrDup(82, " "))
                Next
                Console.BackgroundColor = ConsoleColor.Black
                '''''''''''''''''''''''''''''''''

                If go = True Then
                    'Выдача первых двух карт
                    If h < 2 Then
                        For i = 0 To amountOfPart
                            If amountOfPart = 3 Then
                                If i = 0 Then
                                    TakeCard(2, playersCards, i, 0, denomination)
                                ElseIf i = 1 Then
                                    TakeCard(2, playersCards, i, 0, denomination)
                                ElseIf i = 3 Then 'Карты крупье
                                    TakeCard(2, playersCards, i, 0, denomination)
                                Else
                                    TakeCard(2, playersCards, i, 0, denomination)
                                End If
                            Else
                                If i = 0 Then
                                    TakeCard(2, playersCards, i, 0, denomination)
                                ElseIf i = 2 Then 'Карты крупье
                                    TakeCard(2, playersCards, i, 0, denomination)
                                Else
                                    TakeCard(2, playersCards, i, 0, denomination)
                                End If
                            End If
                        Next
                    End If
                    'Вывод ников игроков
                    i1 = 2
                    j1 = 14
                    For i = 1 To amountOfPart
                        Console.ForegroundColor = ConsoleColor.Cyan
                        Console.SetCursorPosition(i1, j1)
                        If i = 1 Or i = 2 Then
                            If i = 1 Then
                                Console.WriteLine(botsNames(1))
                            Else
                                Console.WriteLine(botsNames(2))
                            End If
                        Else
                            Console.WriteLine(userName)
                        End If
                        j1 += 11
                        Console.ResetColor()
                    Next
                    'Вывод |карты крупье|
                    Console.SetCursorPosition(105, 19)
                    Console.BackgroundColor = ConsoleColor.Gray
                    Console.ForegroundColor = ConsoleColor.Magenta
                    Console.WriteLine("| DEALER'S CARDS |")
                    Console.ResetColor()
                    Console.BackgroundColor = ConsoleColor.Black
                    'Вывод карт
                    i1 = 19

                    'Выводим только 2 первые карты
                    For i = 0 To amountOfPart
                        If i = 0 Then
                            ShowCard(2, i1, 13, playersCards, i, 0, denomination, sumOfCardsOfPlayers)
                        ElseIf i = 1 Then
                            ShowCard(2, i1, 25, playersCards, i, 0, denomination, sumOfCardsOfPlayers)
                        ElseIf i = 3 Then 'Карты крупье
                            ShowCard(2, 105, 21, playersCards, i, 0, denomination, sumOfCardsOfPlayers)
                            'Закрываем одну из карт обложкой
                            helpM = PlayingCardsList(13)
                            For j = 0 To UBound(helpM)
                                Console.SetCursorPosition(105, 21 + j)
                                Console.WriteLine(helpM(j))
                            Next
                        Else
                            'i1 = 19  j1 = 37
                            ShowCard(2, i1, 37, playersCards, i, 0, denomination, sumOfCardsOfPlayers)
                        End If
                    Next
                End If

                'Дальнейшая игра | После двух карт
                'Брать ли ещё карту?
                ''''''''''''''''''''''''''''''''''
                '''
                If go = True Then
                    For i = 0 To amountOfPart - 1
                        If i = 0 Then
                            Pause(3)
                            result = TakeСardOrStop(denomination, i, gameBets)
                            If result Then
                                playersVariants(0) = True
                                'Получение новой карты
                                TakeCard(1, playersCards, i, UBound(playersCards, 1), denomination)
                                Console.BackgroundColor = ConsoleColor.Gray
                                Console.ForegroundColor = ConsoleColor.Black
                                Console.SetCursorPosition(105, 33)
                                Console.WriteLine("[ THE PLAYER {0}, TOOK THE CARD ]", botsNames(1))
                                Console.ResetColor()
                                Console.BackgroundColor = ConsoleColor.Black
                            Else
                                playersVariants(0) = False
                                Console.BackgroundColor = ConsoleColor.Gray
                                Console.ForegroundColor = ConsoleColor.Black
                                Console.SetCursorPosition(105, 33)
                                Console.WriteLine("[ THE PLAYER {0}, STOP ]", botsNames(1))
                                Console.ResetColor()
                                Console.BackgroundColor = ConsoleColor.Black
                            End If
                        ElseIf i = 1 Then
                            Pause(3)
                            result = TakeСardOrStop(denomination, i, gameBets)
                            If result Then
                                playersVariants(1) = True
                                'Получение новой карты
                                TakeCard(1, playersCards, i, UBound(playersCards, 1), denomination)
                                Console.BackgroundColor = ConsoleColor.Gray
                                Console.ForegroundColor = ConsoleColor.Black
                                Console.SetCursorPosition(105, 35)
                                Console.WriteLine("[ THE PLAYER {0}, TOOK THE CARD ]", botsNames(2))
                                Console.ResetColor()
                                Console.BackgroundColor = ConsoleColor.Black
                            Else
                                playersVariants(1) = False
                                Console.BackgroundColor = ConsoleColor.Gray
                                Console.ForegroundColor = ConsoleColor.Black
                                Console.SetCursorPosition(105, 35)
                                Console.WriteLine("[ THE PLAYER {0}, STOP ]", botsNames(2))

                                Console.ResetColor()
                                Console.BackgroundColor = ConsoleColor.Black
                            End If
                        ElseIf i = 2 Then
                            Pause(1)
                            Console.SetCursorPosition(105, 37)
                            Console.BackgroundColor = ConsoleColor.Gray
                            Console.ForegroundColor = ConsoleColor.Black
                            Console.WriteLine("[ {0}, SELECTION: TOOK THE CARD/STOP [+/-] ]", userName)
                            Console.ResetColor()
                            Console.SetCursorPosition(105, 39)
                            Console.BackgroundColor = ConsoleColor.Gray
                            Console.ForegroundColor = ConsoleColor.Black
                            Console.Write("-> ")
                            userAns = Console.ReadLine()
                            Console.ResetColor()
                            Console.BackgroundColor = ConsoleColor.Gray
                            Console.ForegroundColor = ConsoleColor.Black
                            Console.ResetColor()
                            Console.BackgroundColor = ConsoleColor.Black
                            If userAns = "+" Then
                                go = True
                                'Получение новой карты
                                TakeCard(1, playersCards, i, UBound(playersCards, 1), denomination)
                                c = 2
                                playersVariants(2) = True
                                Exit For
                            ElseIf userAns = "-" Then
                                playersVariants(2) = False
                                c = 2
                                go = True
                            Else
                                Console.BackgroundColor = ConsoleColor.Gray
                                str = "[ INVALID VALUE ENTERED ]"
                                LineOutPutCharacterByCharacter(str, 105, 39, 0.04, ConsoleColor.DarkRed)
                                Console.BackgroundColor = ConsoleColor.Black
                                Pause(1)
                                c = 0
                                h = 1
                                go = False
                            End If
                        End If
                        Console.BackgroundColor = ConsoleColor.Black
                    Next
                    'Вывод информации
                    If c = 2 Then
                        If amountOfPart = 3 Then
                            For i = 0 To amountOfPart - 1
                                If i = 0 Then
                                    If playersVariants(i) Then
                                        'Вывод новой карты
                                        ShowCard(1, 46, 13, playersCards, 0, UBound(playersCards, 1), denomination, sumOfCardsOfPlayers)
                                    End If
                                ElseIf i = 1 Then
                                    If playersVariants(i) Then
                                        'Вывод новой карты
                                        ShowCard(1, 46, 25, playersCards, 1, UBound(playersCards, 1), denomination, sumOfCardsOfPlayers)
                                    End If
                                ElseIf i = 2 Then
                                    If playersVariants(i) Then
                                        'Вывод новой карты
                                        ShowCard(1, 46, 37, playersCards, 2, UBound(playersCards, 1), denomination, sumOfCardsOfPlayers)
                                        Pause(1)
                                    End If
                                End If
                            Next
                        End If
                        'Вывод сумма карт игроков
                        'Вывод суммы карт игрок(0)
                        Console.SetCursorPosition(2, 15)
                        Console.ForegroundColor = ConsoleColor.Magenta
                        Console.WriteLine("[ " & sumOfCardsOfPlayers(0) & " ]")
                        Console.ResetColor()

                        'Вывод суммы карт игрока(1)
                        Console.SetCursorPosition(2, 26)
                        Console.ForegroundColor = ConsoleColor.Magenta
                        Console.WriteLine("[ " & sumOfCardsOfPlayers(1) & " ]")
                        Console.ResetColor()

                        'Вывод суммы карт игрока(2)
                        Console.SetCursorPosition(2, 37)
                        Console.ForegroundColor = ConsoleColor.Magenta
                        Console.WriteLine("[ " & sumOfCardsOfPlayers(2) & " ]")
                        Console.ResetColor()
                        go = True
                        Exit Do
                    End If
                End If
            Loop
            go = True
            'Убираем обложку с первой карты крупье
            ShowCard(1, 105, 21, playersCards, 3, 0, denomination, sumOfCardsOfPlayers)

            sum = 0
            For i = 0 To UBound(denomination, 1)
                sum += denomination(i, 3)
            Next

            If sum <= 16 Then
                'Крупье берет карту
                TakeCard(2, playersCards, 3, UBound(playersCards, 1), denomination)
                Console.SetCursorPosition(105, 43)
                Console.BackgroundColor = ConsoleColor.Gray
                str = "[ CROUPIER TOOK THE CARD ]"
                LineOutPutCharacterByCharacter(str, 105, 39, 0.04, ConsoleColor.Magenta)
                Console.BackgroundColor = ConsoleColor.Black
                'Вывод новой карты Крупье
                ShowCard(1, 132, 21, playersCards, 3, UBound(playersCards, 1), denomination, sumOfCardsOfPlayers)
            Else
                Console.SetCursorPosition(105, 43)
                Console.BackgroundColor = ConsoleColor.Gray
                str = "[ CROUPIER STOPPED ]"
                LineOutPutCharacterByCharacter(str, 105, 39, 0.04, ConsoleColor.Magenta)
                Console.BackgroundColor = ConsoleColor.Black
            End If
            sum = 0
            For i = 0 To UBound(denomination, 1)
                sum += denomination(i, 3)
            Next
            sumOfCardsOfPlayers(3) = sum

            'Вывод суммы карт крупье
            Console.SetCursorPosition(122, 19)
            Console.BackgroundColor = ConsoleColor.Gray
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.Write(" SUM = " & sumOfCardsOfPlayers(3))
            Console.ResetColor()
            Console.BackgroundColor = ConsoleColor.Black
            'Вывод результата | Кто победил

            For i = 0 To UBound(winners)
                If sumOfCardsOfPlayers(3) < 21 Then
                    If sumOfCardsOfPlayers(i) > sumOfCardsOfPlayers(3) And sumOfCardsOfPlayers(i) <= 21 Then
                        winners(i) = True
                    Else
                        winners(i) = False
                    End If
                ElseIf sumOfCardsOfPlayers(3) = 21 Then
                    If sumOfCardsOfPlayers(i) = 21 Then
                        winners(i) = True
                    Else
                        winners(i) = False
                    End If
                ElseIf sumOfCardsOfPlayers(3) > 21 Then
                    If sumOfCardsOfPlayers(i) <= 21 Then
                        winners(i) = True
                    Else
                        winners(i) = False
                    End If
                End If
            Next

            For i = 0 To UBound(winners)
                If winners(i) Then
                    str = "[ WINNING ]"
                    i1 = 65
                    If i = 0 Then
                        j1 = 18
                        Console.BackgroundColor = ConsoleColor.Black
                        LineOutPutCharacterByCharacter(str, i1, j1, 0.04, ConsoleColor.Magenta)
                    ElseIf i = 1 Then
                        j1 = 30
                        Console.BackgroundColor = ConsoleColor.Black
                        LineOutPutCharacterByCharacter(str, i1, j1, 0.04, ConsoleColor.Magenta)
                    ElseIf i = 2 Then
                        j1 = 42
                        Console.BackgroundColor = ConsoleColor.Black
                        LineOutPutCharacterByCharacter(str, i1, j1, 0.04, ConsoleColor.Magenta)
                        blackJack_coins += gameBets(2)
                    End If
                End If
                If Not winners(i) Then
                    str = "[ LOSING ]"
                    If i = 0 Then
                        Console.BackgroundColor = ConsoleColor.Black
                        LineOutPutCharacterByCharacter(str, 65, 18, 0.04, ConsoleColor.Magenta)
                    ElseIf i = 1 Then
                        Console.BackgroundColor = ConsoleColor.Black
                        LineOutPutCharacterByCharacter(str, 65, 30, 0.04, ConsoleColor.Magenta)
                    ElseIf i = 2 Then
                        Console.BackgroundColor = ConsoleColor.Black
                        LineOutPutCharacterByCharacter(str, 65, 42, 0.04, ConsoleColor.Magenta)
                        blackJack_coins -= gameBets(2)
                    End If
                End If
            Next
            Console.BackgroundColor = ConsoleColor.Black

            Console.SetCursorPosition(105, 43)
            Console.BackgroundColor = ConsoleColor.Gray
            Console.ForegroundColor = ConsoleColor.Black
            f += 1
            Console.Write("PLAYING MORE? [+/-] [GAMES LEFT - {0}]: ", amountOfAllGames - f)
            Console.ResetColor()
            Console.BackgroundColor = ConsoleColor.Gray
            Console.ForegroundColor = ConsoleColor.Black
            userAns = Console.ReadLine()
            Console.ResetColor()
            Console.BackgroundColor = ConsoleColor.Black
            If userAns = "-" Then
                Exit Do
            End If

            If f <> amountOfAllGames Then
                go = False
            Else
                Exit Do
            End If
        Loop Until f = amountOfAllGames Or blackJack_coins <= 0

        Console.Clear()
        'Череп
        Console.ForegroundColor = ConsoleColor.Cyan
        DisplayingPictureFromSide(str2, 37, 5, speed - 0.0054)
        Console.ResetColor()

        Dim sp_num As Integer

        If blackJack_coins <= 0 Then
            str = "THE GAME IS OVER! THE PLAYER HAVE NO BISCUITS IN THE ACCOUNT.."
            sp_num = 1
        ElseIf f <= amountOfAllGames - 1 Then
            str = "THE GAME IS OVER! PIP STOPPED THE GAME .."
            sp_num = 1
        ElseIf f = amountOfAllGames And playerCoins > 0 Then
            If Not done_first_season_tasks(9) Then
                For i = 0 To UBound(card_and_standart_games, 2)
                    If Not card_and_standart_games(0, i) Then
                        card_and_standart_games(0, i) = True
                        Exit For
                    End If
                Next
            End If

            str = "THE GAME IS OVER! ALL GAMES GONE!"
            sp_num = 0
            playerCoins += blackJack_coins
        End If

        'Рамка
        Console.SetCursorPosition(0, 28)
        Console.ForegroundColor = ConsoleColor.Cyan
        ImageOutputLineByLine(frame_pic, 0.015, 15)
        Console.ResetColor()

        LineOutPutCharacterByCharacter(str, 18, 30, 0.015, ConsoleColor.Cyan)
        Pause(1)
        str = "THANKS TO ALL WHO COME TO THE GAME .."
        LineOutPutCharacterByCharacter(str, 18, 32, 0.015, ConsoleColor.White)
        Pause(1)
        str = "EVERYONE IS NOT SICK AND SEE YOU SOON!"
        LineOutPutCharacterByCharacter(str, 18, 34, 0.015, ConsoleColor.White)
        str = "[ PRESS ANY KEY TO CONTINUE ]"
        LineOutPutCharacterByCharacter(str, 18, 36, 0.015, ConsoleColor.Yellow)
        Console.ReadKey()

        StandartWin(sp_num, enemyName, icon, m, chapter_num, levelNum)
    End Sub

    Sub MapFirstChapter()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim mainPartMap() As String =
            {
                "═════════╬",
                "║   ║    ║",
                "║   ║╬═══║",
                "║═══╬║   ║",
                "║    ║   ║",
                "╬═════════"
            }

        Dim bossIcon() As String =
            {
                "         ││         ",
                "     ────││───╬     ",
                "     │╬═══════│     ",
                "     │║  ││  ║│     ",
                "─────────────║│─────",
                "     │║  ││  ║│     ",
                "     │║  ││  ║│     ",
                "     │════════│     ",
                "     ╬───││────     ",
                "         ││         "
            }

        Dim stick() As String =
            {
                "│",
                "│",
                "│"
            }

        Dim infoBar() As String =
            {
                "[ YELLOW - LEVEL NOT PASSED ]",
                "[ BLUE - NEXT OPEN LEVEL ]",
                "[ GREEN - LEVEL PASSED ]",
                "[ CHOOSE BLUE LEVEL TO PASS ]",
                "[ ENTER ""0"" (ZERO) TO RETURN ]"
            }

        Dim chapter1Pic() As String =
            {
                "╔══╗ ╔╗╔╗ ╔══╗ ╔═══╗ ╔════╗ ╔═══╗ ╔═══╗  ╔╗",
                "║╔═╝ ║║║║ ║╔╗║ ║╔═╗║ ╚═╗╔═╝ ║╔══╝ ║╔═╗║ ╔╝║",
                "║║   ║╚╝║ ║╚╝║ ║╚═╝║  ─║║─  ║╚══╗ ║╚═╝║ ╚╗║",
                "║║   ║╔╗║ ║╔╗║ ║╔══╝  ─║║─  ║╔══╝ ║╔╗╔╝  ║║",
                "║╚═╗ ║║║║ ║║║║ ║║      ║║   ║╚══╗ ║║║║   ║║",
                "╚══╝ ╚╝╚╝ ╚╝╚╝ ╚╝     ─╚╝─  ╚═══╝ ╚╝╚╝   ╚╝"
            }

        Dim stick2() As String =
            {
                "═",
                "█",
                "█",
                "█",
                "█",
                "═"
            }

        Dim l As Integer

        Dim passTheCube As Boolean

        Dim str, userAns As String

        Do
            'Вывод карты первой главы
            '[1] Куб
            If doneLevelsFromCharpet1(0) Then
                Console.ForegroundColor = ConsoleColor.Green
            Else
                passTheCube = True
                Console.ForegroundColor = ConsoleColor.Blue
            End If
            DisplayingPictureFromSide(mainPartMap, 20, 11, speed)
            Console.ResetColor()
            '[2] Куб
            If doneLevelsFromCharpet1(1) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf Not passTheCube Then
                Console.ForegroundColor = ConsoleColor.Blue
                passTheCube = True
            Else
                Console.ForegroundColor = ConsoleColor.Yellow
            End If
            DisplayingPictureFromSide(mainPartMap, 50, 11, speed)
            Console.ResetColor()
            '[3] Куб
            If doneLevelsFromCharpet1(2) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf Not passTheCube Then
                Console.ForegroundColor = ConsoleColor.Blue
                passTheCube = True
            Else
                Console.ForegroundColor = ConsoleColor.Yellow
            End If
            DisplayingPictureFromSide(mainPartMap, 80, 11, speed)
            Console.ResetColor()
            '[4] Куб
            If doneLevelsFromCharpet1(3) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf Not passTheCube Then
                Console.ForegroundColor = ConsoleColor.Blue
                passTheCube = True
            Else
                Console.ForegroundColor = ConsoleColor.Yellow
            End If
            DisplayingPictureFromSide(mainPartMap, 110, 11, speed)
            Console.ResetColor()
            'Вывод соединений под кубами
            l = 25
            For i = 1 To 4
                DisplayingPictureFromSide(stick, l, 18, speed)
                l += 30
                Pause(0.2)
            Next
            l = 27
            For i = 1 To 87
                Console.SetCursorPosition(l, 20)
                Console.Write("─")
                l += 1
                Pause(0.01)
            Next
            l = 21
            For i = 1 To 4
                Console.SetCursorPosition(69, l)
                Console.Write("││")
                l += 1
                Pause(0.05)
            Next
            '[5] Иконка ключа
            passTheCube = False
            For i = 0 To UBound(doneLevelsFromCharpet1)
                If Not doneLevelsFromCharpet1(i) Then
                    passTheCube = True
                    Exit For
                End If
            Next
            If passTheCube Then
                Console.ForegroundColor = ConsoleColor.White
            Else
                Console.ForegroundColor = ConsoleColor.Green
            End If
            DisplayingPictureFromSide(bossIcon, 60, 26, speed)

            'Номера кубов и ключа
            l = 29
            Console.ForegroundColor = ConsoleColor.DarkCyan
            For i = 0 To 4
                If i <> 4 Then
                    Console.SetCursorPosition(l, 9)
                    Console.WriteLine("[{0}]", i + 1)
                    l += 30
                End If
            Next
            Console.ResetColor()

            'Вывод надписи |chapter1|
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.SetCursorPosition(0, 1)
            ImageOutputLineByLine(chapter1Pic, 0.1, 50)
            Console.ResetColor()

            'Линия через надпись
            Console.ForegroundColor = ConsoleColor.Cyan
            For i = 1 To 48
                DisplayingPictureFromSide(stick2, i, 1, 0.001)
            Next
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Cyan
            For i = 94 To 138
                DisplayingPictureFromSide(stick2, i, 1, 0.001)
            Next
            Console.ResetColor()

            'Вывод информационных сообщений
            l = 28
            For i = 0 To UBound(infoBar)
                str = infoBar(i)
                LineOutPutCharacterByCharacter(str, 20, l, speed, ConsoleColor.Yellow)
                l += 2
            Next

            Console.SetCursorPosition(65, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            Select Case userAns
                Case 0
                    'Возвращение в меню выбора главы
                    DungeonMenuPath()
                Case 1
                    'Переход в первый уровень
                    Level(1, 1)
                Case 2
                    If doneLevelsFromCharpet1(0) Then
                        Level(2, 1)
                    Else
                        speed = 0
                        str = "[ LEVEL NOT AVAILABLE | REQUIRED LEVEL FULFILLED: LEVEL 1 ]"
                        LineOutPutCharacterByCharacter(str, 40, 38, 0.03, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case 3
                    If doneLevelsFromCharpet1(1) Then
                        Level(3, 1)
                    Else
                        speed = 0
                        str = "[ LEVEL NOT AVAILABLE | REQUIRED LEVEL FULFILLED: LEVEL 2 ]"
                        LineOutPutCharacterByCharacter(str, 40, 38, 0.03, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case 4
                    If doneLevelsFromCharpet1(2) Then
                        Level(4, 1)
                    Else
                        speed = 0
                        str = "[ LEVEL NOT AVAILABLE | REQUIRED LEVEL FULFILLED: LEVEL 3 ]"
                        LineOutPutCharacterByCharacter(str, 40, 38, 0.03, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case Else
                    speed = 0
                    str = "[ INVALID VALUE ]"
                    LineOutPutCharacterByCharacter(str, 61, 38, 0.03, ConsoleColor.Red)
                    Pause(2)
                    Console.Clear()
            End Select
        Loop
    End Sub
    Sub MapSecondChapter()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim mainPartMap() As String =
            {
                "═════════╬",
                "║   ║    ║",
                "║   ║╬═══║",
                "║═══╬║   ║",
                "║    ║   ║",
                "╬═════════"
            }

        Dim bossIcon() As String =
            {
                "         ││         ",
                "     ────││───╬     ",
                "     │╬═══════│     ",
                "     │║  ││  ║│     ",
                "─────────────║│─────",
                "     │║  ││  ║│     ",
                "     │║  ││  ║│     ",
                "     │════════│     ",
                "     ╬───││────     ",
                "         ││         "
            }

        Dim stick() As String =
            {
                "│",
                "│",
                "│"
            }

        Dim infoBar() As String =
            {
                "[ YELLOW - LEVEL NOT PASSED ]",
                "[ BLUE - NEXT OPEN LEVEL ]",
                "[ GREEN - LEVEL PASSED ]",
                "[ CHOOSE BLUE LEVEL TO PASS ]",
                "[ ENTER ""0"" (ZERO) TO RETURN ]"
            }

        Dim chapter2Pic() As String =
            {
                "╔══╗ ╔╗╔╗ ╔══╗ ╔═══╗ ╔════╗ ╔═══╗ ╔═══╗ ╔══╗",
                "║╔═╝ ║║║║ ║╔╗║ ║╔═╗║ ╚═╗╔═╝ ║╔══╝ ║╔═╗║ ╚═╗║",
                "║║   ║╚╝║ ║╚╝║ ║╚═╝║  ─║║─  ║╚══╗ ║╚═╝║ ╔═╝║",
                "║║   ║╔╗║ ║╔╗║ ║╔══╝  ─║║─  ║╔══╝ ║╔╗╔╝ ║╔═╝",
                "║╚═╗ ║║║║ ║║║║ ║║      ║║   ║╚══╗ ║║║║  ║╚═╗",
                "╚══╝ ╚╝╚╝ ╚╝╚╝ ╚╝     ─╚╝─  ╚═══╝ ╚╝╚╝  ╚══╝"
            }

        Dim stick2() As String =
            {
                "═",
                "█",
                "█",
                "█",
                "█",
                "═"
            }

        Dim l As Integer

        Dim passTheCube As Boolean

        Dim str, userAns As String

        Do
            'Вывод карты первой главы
            '[1] Куб
            If doneLevelsFromCharpet2(0) Then
                Console.ForegroundColor = ConsoleColor.Green
            Else
                passTheCube = True
                Console.ForegroundColor = ConsoleColor.Blue
            End If
            DisplayingPictureFromSide(mainPartMap, 20, 11, speed)
            Console.ResetColor()
            '[2] Куб
            If doneLevelsFromCharpet2(1) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf Not passTheCube Then
                Console.ForegroundColor = ConsoleColor.Blue
                passTheCube = True
            Else
                Console.ForegroundColor = ConsoleColor.Yellow
            End If
            DisplayingPictureFromSide(mainPartMap, 50, 11, speed)
            Console.ResetColor()
            '[3] Куб
            If doneLevelsFromCharpet2(2) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf Not passTheCube Then
                Console.ForegroundColor = ConsoleColor.Blue
                passTheCube = True
            Else
                Console.ForegroundColor = ConsoleColor.Yellow
            End If
            DisplayingPictureFromSide(mainPartMap, 80, 11, speed)
            Console.ResetColor()
            '[4] Куб
            If doneLevelsFromCharpet2(3) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf Not passTheCube Then
                Console.ForegroundColor = ConsoleColor.Blue
                passTheCube = True
            Else
                Console.ForegroundColor = ConsoleColor.Yellow
            End If
            DisplayingPictureFromSide(mainPartMap, 110, 11, speed)
            Console.ResetColor()
            'Вывод соединений под кубами
            l = 25
            For i = 1 To 4
                DisplayingPictureFromSide(stick, l, 18, speed)
                l += 30
                Pause(0.2)
            Next
            l = 27
            For i = 1 To 87
                Console.SetCursorPosition(l, 20)
                Console.Write("─")
                l += 1
                Pause(0.01)
            Next
            l = 21
            For i = 1 To 4
                Console.SetCursorPosition(69, l)
                Console.Write("││")
                l += 1
                Pause(0.05)
            Next
            '[5] Иконка ключа
            passTheCube = False
            For i = 0 To UBound(doneLevelsFromCharpet1)
                If Not doneLevelsFromCharpet2(i) Then
                    passTheCube = True
                    Exit For
                End If
            Next
            If passTheCube Then
                Console.ForegroundColor = ConsoleColor.White
            Else
                Console.ForegroundColor = ConsoleColor.Green
            End If
            DisplayingPictureFromSide(bossIcon, 60, 26, speed)

            'Номера кубов и ключа
            l = 29
            Console.ForegroundColor = ConsoleColor.DarkCyan
            For i = 0 To 4
                If i <> 4 Then
                    Console.SetCursorPosition(l, 9)
                    Console.WriteLine("[{0}]", i + 1)
                    l += 30
                End If
            Next
            Console.ResetColor()

            'Вывод надписи |chapter2|
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.SetCursorPosition(0, 1)
            ImageOutputLineByLine(chapter2Pic, 0.1, 50)
            Console.ResetColor()

            'Линия через надпись
            Console.ForegroundColor = ConsoleColor.Cyan
            For i = 1 To 48
                DisplayingPictureFromSide(stick2, i, 1, 0.001)
            Next
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Cyan
            For i = 94 To 138
                DisplayingPictureFromSide(stick2, i, 1, 0.001)
            Next
            Console.ResetColor()

            'Вывод информационных сообщений
            l = 28
            For i = 0 To UBound(infoBar)
                str = infoBar(i)
                LineOutPutCharacterByCharacter(str, 20, l, speed, ConsoleColor.Yellow)
                l += 2
            Next

            Console.SetCursorPosition(65, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            Select Case userAns
                Case 0
                    'Возвращение в меню выбора главы
                    DungeonMenuPath()
                Case 1
                    'Переход в первый уровень
                    Level(1, 2)
                Case 2
                    If doneLevelsFromCharpet1(0) Then
                        Level(2, 2)
                    Else
                        speed = 0
                        str = "[ LEVEL NOT AVAILABLE | REQUIRED LEVEL FULFILLED: LEVEL 1 ]"
                        LineOutPutCharacterByCharacter(str, 40, 38, 0.03, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case 3
                    If doneLevelsFromCharpet1(1) Then
                        Level(3, 2)
                    Else
                        speed = 0
                        str = "[ LEVEL NOT AVAILABLE | REQUIRED LEVEL FULFILLED: LEVEL 2 ]"
                        LineOutPutCharacterByCharacter(str, 40, 38, 0.03, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case 4
                    If doneLevelsFromCharpet1(2) Then
                        Level(4, 2)
                    Else
                        speed = 0
                        str = "[ LEVEL NOT AVAILABLE | REQUIRED LEVEL FULFILLED: LEVEL 3 ]"
                        LineOutPutCharacterByCharacter(str, 40, 38, 0.03, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case Else
                    speed = 0
                    str = "[ INVALID VALUE ]"
                    LineOutPutCharacterByCharacter(str, 61, 38, 0.03, ConsoleColor.Red)
                    Pause(2)
                    Console.Clear()
            End Select
        Loop
    End Sub
    Sub MapFirdChapter()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim mainPartMap() As String =
            {
                "═════════╬",
                "║   ║    ║",
                "║   ║╬═══║",
                "║═══╬║   ║",
                "║    ║   ║",
                "╬═════════"
            }

        Dim keyIcon() As String =
            {
                "         ││         ",
                "     ────││───╬     ",
                "     │╬═══════│     ",
                "     │║  ││  ║│     ",
                "─────────────║│─────",
                "     │║  ││  ║│     ",
                "     │║  ││  ║│     ",
                "     │════════│     ",
                "     ╬───││────     ",
                "         ││         "
            }

        Dim stick() As String =
            {
                "│",
                "│",
                "│"
            }

        Dim infoBar() As String =
            {
                "[ YELLOW - LEVEL NOT PASSED ]",
                "[ BLUE - NEXT OPEN LEVEL ]",
                "[ GREEN - LEVEL PASSED ]",
                "[ CHOOSE BLUE LEVEL TO PASS ]",
                "[ ENTER ""0"" (ZERO) TO RETURN ]"
            }

        Dim chapter3Pic() As String =
            {
                "╔══╗ ╔╗╔╗ ╔══╗ ╔═══╗ ╔════╗ ╔═══╗ ╔═══╗ ╔══╗",
                "║╔═╝ ║║║║ ║╔╗║ ║╔═╗║ ╚═╗╔═╝ ║╔══╝ ║╔═╗║ ╚═╗║",
                "║║   ║╚╝║ ║╚╝║ ║╚═╝║  ─║║─  ║╚══╗ ║╚═╝║ ╔═╝║",
                "║║   ║╔╗║ ║╔╗║ ║╔══╝  ─║║─  ║╔══╝ ║╔╗╔╝ ╚═╗║",
                "║╚═╗ ║║║║ ║║║║ ║║      ║║   ║╚══╗ ║║║║  ╔═╝║",
                "╚══╝ ╚╝╚╝ ╚╝╚╝ ╚╝     ─╚╝─  ╚═══╝ ╚╝╚╝  ╚══╝"
            }

        Dim stick2() As String =
            {
                "═",
                "█",
                "█",
                "█",
                "█",
                "═"
            }

        Dim l As Integer

        Dim passTheCube As Boolean

        Dim str, userAns As String

        Do
            'Вывод карты первой главы
            '[1] Куб
            If doneLevelsFromCharpet3(0) Then
                Console.ForegroundColor = ConsoleColor.Green
            Else
                passTheCube = True
                Console.ForegroundColor = ConsoleColor.Blue
            End If
            DisplayingPictureFromSide(mainPartMap, 20, 11, speed)
            Console.ResetColor()
            '[2] Куб
            If doneLevelsFromCharpet3(1) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf Not passTheCube Then
                Console.ForegroundColor = ConsoleColor.Blue
                passTheCube = True
            Else
                Console.ForegroundColor = ConsoleColor.Yellow
            End If
            DisplayingPictureFromSide(mainPartMap, 50, 11, speed)
            Console.ResetColor()
            '[3] Куб
            If doneLevelsFromCharpet3(2) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf Not passTheCube Then
                Console.ForegroundColor = ConsoleColor.Blue
                passTheCube = True
            Else
                Console.ForegroundColor = ConsoleColor.Yellow
            End If
            DisplayingPictureFromSide(mainPartMap, 80, 11, speed)
            Console.ResetColor()
            '[4] Куб
            If doneLevelsFromCharpet3(3) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf Not passTheCube Then
                Console.ForegroundColor = ConsoleColor.Blue
                passTheCube = True
            Else
                Console.ForegroundColor = ConsoleColor.Yellow
            End If
            DisplayingPictureFromSide(mainPartMap, 110, 11, speed)
            Console.ResetColor()
            'Вывод соединений под кубами
            l = 25
            For i = 1 To 4
                DisplayingPictureFromSide(stick, l, 18, speed)
                l += 30
                Pause(0.2)
            Next
            l = 27
            For i = 1 To 87
                Console.SetCursorPosition(l, 20)
                Console.Write("─")
                l += 1
                Pause(0.01)
            Next
            l = 21
            For i = 1 To 4
                Console.SetCursorPosition(69, l)
                Console.Write("││")
                l += 1
                Pause(0.05)
            Next
            '[5] Иконка ключа
            passTheCube = False
            For i = 0 To UBound(doneLevelsFromCharpet1)
                If Not doneLevelsFromCharpet1(i) Then
                    passTheCube = True
                    Exit For
                End If
            Next
            If passTheCube Then
                Console.ForegroundColor = ConsoleColor.White
            Else
                Console.ForegroundColor = ConsoleColor.Green
            End If
            DisplayingPictureFromSide(keyIcon, 60, 26, speed)

            'Номера кубов и ключа
            l = 29
            Console.ForegroundColor = ConsoleColor.DarkCyan
            For i = 0 To 4
                If i <> 4 Then
                    Console.SetCursorPosition(l, 9)
                    Console.WriteLine("[{0}]", i + 1)
                    l += 30
                End If
            Next
            Console.ResetColor()

            'Вывод надписи |chapter3|
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.SetCursorPosition(0, 1)
            ImageOutputLineByLine(chapter3Pic, 0.1, 50)
            Console.ResetColor()

            'Линия через надпись
            Console.ForegroundColor = ConsoleColor.Cyan
            For i = 1 To 48
                DisplayingPictureFromSide(stick2, i, 1, 0.001)
            Next
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Cyan
            For i = 94 To 138
                DisplayingPictureFromSide(stick2, i, 1, 0.001)
            Next
            Console.ResetColor()

            'Вывод информационных сообщений
            l = 28
            For i = 0 To UBound(infoBar)
                str = infoBar(i)
                LineOutPutCharacterByCharacter(str, 20, l, speed, ConsoleColor.Yellow)
                l += 2
            Next

            Console.SetCursorPosition(65, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            Select Case userAns
                Case 0
                    'Возвращение в меню выбора главы
                    DungeonMenuPath()
                Case 1
                    'Переход в первый уровень
                    Level(1, 1)
                Case 2
                    If doneLevelsFromCharpet1(0) Then
                        Level(2, 3)
                    Else
                        speed = 0
                        str = "[ LEVEL NOT AVAILABLE | REQUIRED LEVEL FULFILLED: LEVEL 1 ]"
                        LineOutPutCharacterByCharacter(str, 40, 38, 0.03, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case 3
                    If doneLevelsFromCharpet1(1) Then
                        Level(3, 3)
                    Else
                        speed = 0
                        str = "[ LEVEL NOT AVAILABLE | REQUIRED LEVEL FULFILLED: LEVEL 2 ]"
                        LineOutPutCharacterByCharacter(str, 40, 38, 0.03, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case 4
                    If doneLevelsFromCharpet1(2) Then
                        Level(4, 3)
                    Else
                        speed = 0
                        str = "[ LEVEL NOT AVAILABLE | REQUIRED LEVEL FULFILLED: LEVEL 3 ]"
                        LineOutPutCharacterByCharacter(str, 40, 38, 0.03, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case Else
                    speed = 0
                    str = "[ INVALID VALUE ]"
                    LineOutPutCharacterByCharacter(str, 61, 38, 0.03, ConsoleColor.Red)
                    Pause(2)
                    Console.Clear()
            End Select
        Loop
    End Sub
    Sub Display_fird_chapter()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim secondChapter_pic() As String =
            {
                "══════════     ╔══╗   ╔══╗   ╔═══╗   ╔══╗      ╔══╗   ╔╗╔╗   ╔══╗   ╔═══╗   ╔════╗   ╔═══╗   ╔═══╗",
                "║ ║║  ║║ ║     ║╔═╝   ╚╗╔╝   ║╔═╗║   ║╔╗╚╗     ║╔═╝   ║║║║   ║╔╗║   ║╔═╗║   ╚═╗╔═╝   ║╔══╝   ║╔═╗║",
                "║ ║║  ║║ ║     ║╚═╗    ║║    ║╚═╝║   ║║╚╗║     ║║     ║╚╝║   ║╚╝║   ║╚═╝║     ║║     ║╚══╗   ║╚═╝║",
                "║──║──║──║     ║╔═╝    ║║    ║╔╗╔╝   ║║ ║║     ║║     ║╔╗║   ║╔╗║   ║╔══╝     ║║     ║╔══╝   ║╔╗╔╝",
                "║ ║║  ║║ ║     ║║     ╔╝╚╗   ║║║║    ║╚═╝║     ║╚═╗   ║║║║   ║║║║   ║║───     ║║     ║╚══╗   ║║║║ ",
                "══════════     ╚╝     ╚══╝   ╚╝╚╝    ╚═══╝     ╚══╝   ╚╝╚╝   ╚╝╚╝   ╚╝───     ╚╝     ╚═══╝   ╚╝╚╝ "
            }

        'Вывод изображения
        Console.SetCursorPosition(0, 14)
        Console.ForegroundColor = ConsoleColor.Yellow
        ImageOutputLineByLine(secondChapter_pic, 0.06, 16)
        Console.ResetColor()

        Pause(2)

        'Переход в карту первой главы
        MapFirdChapter()
    End Sub
    Sub Display_second_chapter()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim secondChapter_pic() As String =
            {
                "══════════     ╔══╗   ╔═══╗   ╔══╗   ╔══╗   ╔╗─╔╗   ╔══╗       ╔══╗   ╔╗╔╗   ╔══╗   ╔═══╗   ╔════╗   ╔═══╗   ╔═══╗",
                "║ ║║  ║║ ║     ║╔═╝   ║╔══╝   ║╔═╝   ║╔╗║   ║╚═╝║   ║╔╗╚╗      ║╔═╝   ║║║║   ║╔╗║   ║╔═╗║   ╚═╗╔═╝   ║╔══╝   ║╔═╗║",
                "║ ║║  ║║ ║     ║╚═╗   ║╚══╗   ║║     ║║║║   ║╔╗─║   ║║╚╗║      ║║     ║╚╝║   ║╚╝║   ║╚═╝║     ║║     ║╚══╗   ║╚═╝║",
                "║──║──║──║     ╚═╗║   ║╔══╝   ║║     ║║║║   ║║╚╗║   ║║─║║      ║║     ║╔╗║   ║╔╗║   ║╔══╝     ║║     ║╔══╝   ║╔╗╔╝",
                "║ ║║  ║║ ║     ╔═╝║   ║╚══╗   ║╚═╗   ║╚╝║   ║║─║║   ║╚═╝║      ║╚═╗   ║║║║   ║║║║   ║║───     ║║     ║╚══╗   ║║║║ ",
                "══════════     ╚══╝   ╚═══╝   ╚══╝   ╚══╝   ╚╝─╚╝   ╚═══╝      ╚══╝   ╚╝╚╝   ╚╝╚╝   ╚╝───     ╚╝     ╚═══╝   ╚╝╚╝ "
            }

        'Вывод изображения
        Console.SetCursorPosition(0, 14)
        Console.ForegroundColor = ConsoleColor.Yellow
        ImageOutputLineByLine(secondChapter_pic, 0.06, 16)
        Console.ResetColor()

        Pause(2)

        'Переход в карту первой главы
        MapSecondChapter()
    End Sub
    Sub DisplayFisrtChapter()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim firstChapterPic() As String =
            {
                "══════════     ╔══╗   ╔══╗   ╔═══╗   ╔══╗   ╔════╗     ╔══╗   ╔╗╔╗   ╔══╗   ╔═══╗   ╔════╗   ╔═══╗   ╔═══╗",
                "║ ║║  ║║ ║     ║╔═╝   ╚╗╔╝   ║╔═╗║   ║╔═╝   ╚═╗╔═╝     ║╔═╝   ║║║║   ║╔╗║   ║╔═╗║   ╚═╗╔═╝   ║╔══╝   ║╔═╗║",
                "║ ║║  ║║ ║     ║╚═╗    ║║    ║╚═╝║   ║╚═╗     ║║       ║║     ║╚╝║   ║╚╝║   ║╚═╝║     ║║     ║╚══╗   ║╚═╝║",
                "║──║──║──║     ║╔═╝    ║║    ║╔╗╔╝   ╚═╗║     ║║       ║║     ║╔╗║   ║╔╗║   ║╔══╝     ║║     ║╔══╝   ║╔╗╔╝",
                "║ ║║  ║║ ║     ║║     ╔╝╚╗   ║║║║    ╔═╝║     ║║       ║╚═╗   ║║║║   ║║║║   ║║        ║║     ║╚══╗   ║║║║ ",
                "══════════     ╚╝     ╚══╝   ╚╝╚╝    ╚══╝     ╚╝       ╚══╝   ╚╝╚╝   ╚╝╚╝   ╚╝        ╚╝     ╚═══╝   ╚╝╚╝ "
            }

        'Вывод изображения
        Console.SetCursorPosition(0, 14)
        Console.ForegroundColor = ConsoleColor.Yellow
        ImageOutputLineByLine(firstChapterPic, 0.06, 16)
        Console.ResetColor()

        Pause(2)

        'Переход в карту первой главы
        MapFirstChapter()
    End Sub
    Sub EnemysFromCharpet1(ByRef m() As String, ByRef enemyName As String, ByRef Name As String)
        Dim rndNum As Integer

        Dim knight() As String =
            {
                " ─╬     ",
                "──│    │",
                " ╬═══╬ │",
                "╬═╬┼┼║ │",
                "║│║══╬ ┼",
                "╬═╬══╬  "
            }

        Dim magician() As String =
            {
                "   ──     ",
                "  ─────   ",
                " ─╬═══╬─ ┼",
                "──║ ││║─ │",
                "  ╬ ─ ╬  │",
                "  ╬═══╬  │"
            }

        Dim mrSlime() As String =
            {
                "   ──   ",
                "  ───│  ",
                " ─╬─╬─│ ",
                "  ─═─── ",
                "│──────│",
                "────────"
            }

        Dim magicChest() As String =
            {
                "  ╬    ╬",
                "╬    ╬  ",
                " ══════ ",
                "║══││══║",
                "║──────║",
                "════════"
            }

        Randomize()
        rndNum = Rnd() * (4 - 1) + 1

        Select Case rndNum
            Case 1
                enemyName = "K N I G H T"
                For i = 0 To UBound(m)
                    m(i) = knight(i)
                Next
                Name = "knight"
            Case 2
                enemyName = "M A G I C I A N"
                For i = 0 To UBound(m)
                    m(i) = magician(i)
                Next
                Name = "magician"
            Case 3
                enemyName = "M R  S L I M E"
                For i = 0 To UBound(m)
                    m(i) = mrSlime(i)
                Next
                Name = "mrSlime"
            Case 4
                enemyName = "M A G I C  C H E S T"
                For i = 0 To UBound(m)
                    m(i) = magicChest(i)
                Next
                Name = "magicchest"
        End Select
    End Sub
    Sub DungeonMenuPath()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.0035
        End If


        Dim gates1() As String =
            {
                "╬╬══════════╬╬╬╬══════════╬╬",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║    ╔╗    ║══║          ║║",
                "║║   ╔╝║    ║══║          ║║",
                "║║   ╚╗║    ║══║          ║║",
                "║║    ║║    ║══║          ║║",
                "║║   ╔╝╚╗   ║══║          ║║",
                "║║   ╚══╝   ║══║          ║║",
                "║║          ║══║          ║║",
                "║║         ┼────┼         ║║",
                "║║         │║══╬│         ║║",
                "║║         │╬══║│         ║║",
                "║║         ┼────┼         ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "╬╬══════════╬╬╬╬══════════╬╬"
            }

        Dim gates2() As String =
            {
                "╬╬══════════╬╬╬╬══════════╬╬",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║  ╔═══╗   ║══║          ║║",
                "║║  ║╔═╗║   ║══║          ║║",
                "║║  ╚╝╔╝║   ║══║          ║║",
                "║║  ╔═╝╔╝   ║══║          ║║",
                "║║  ║ ╚═╗   ║══║          ║║",
                "║║  ╚═══╝   ║══║          ║║",
                "║║          ║══║          ║║",
                "║║         ┼────┼         ║║",
                "║║         │║══╬│         ║║",
                "║║         │╬══║│         ║║",
                "║║         ┼────┼         ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "╬╬══════════╬╬╬╬══════════╬╬"
            }

        Dim gates3() As String =
            {
                "╬╬══════════╬╬╬╬══════════╬╬",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║  ╔═══╗   ║══║          ║║",
                "║║  ║╔═╗║   ║══║          ║║",
                "║║  ╚╝╔╝║   ║══║          ║║",
                "║║  ╔╗╚╗║   ║══║          ║║",
                "║║  ║╚═╝║   ║══║          ║║",
                "║║  ╚═══╝   ║══║          ║║",
                "║║          ║══║          ║║",
                "║║         ┼────┼         ║║",
                "║║         │║══╬│         ║║",
                "║║         │╬══║│         ║║",
                "║║         ┼────┼         ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "║║          ║══║          ║║",
                "╬╬══════════╬╬╬╬══════════╬╬"
            }

        Dim chapter3Pic() As String =
            {
                "╔══╦╗╔╦══╦═══╦════╦═══╦═══╗  ╔╗     ╔══╦╗╔╦══╦═══╦════╦═══╦═══╗ ╔══╗     ╔══╦╗╔╦══╦═══╦════╦═══╦═══╗ ╔══╗",
                "║╔═╣║║║╔╗║╔═╗╠═╗╔═╣╔══╣╔═╗║ ╔╝║     ║╔═╣║║║╔╗║╔═╗╠═╗╔═╣╔══╣╔═╗║ ╚═╗║     ║╔═╣║║║╔╗║╔═╗╠═╗╔═╣╔══╣╔═╗║ ╚═╗║",
                "║║─║╚╝║╚╝║╚═╝║─║║─║╚══╣╚═╝║ ╚╗║     ║║─║╚╝║╚╝║╚═╝║─║║─║╚══╣╚═╝║ ╔═╝║     ║║─║╚╝║╚╝║╚═╝║─║║─║╚══╣╚═╝║ ╔═╝║",
                "║║─║╔╗║╔╗║╔══╝─║║─║╔══╣╔╗╔╝  ║║     ║║─║╔╗║╔╗║╔══╝─║║─║╔══╣╔╗╔╝ ║╔═╝     ║║─║╔╗║╔╗║╔══╝─║║─║╔══╣╔╗╔╝ ╚═╗║",
                "║╚═╣║║║║║║║────║║─║╚══╣║║║   ║║     ║╚═╣║║║║║║║────║║─║╚══╣║║║  ║╚═╗     ║╚═╣║║║║║║║────║║─║╚══╣║║║  ╔═╝║",
                "╚══╩╝╚╩╝╚╩╝────╚╝─╚═══╩╝╚╝   ╚╝     ╚══╩╝╚╩╝╚╩╝────╚╝─╚═══╩╝╚╝  ╚══╝     ╚══╩╝╚╩╝╚╩╝────╚╝─╚═══╩╝╚╝  ╚══╝"
            }

        Dim pattern() As String =
            {
                "═",
                "█",
                "█",
                "═"
            }

        Dim pattern2() As String =
            {
                "┼────┼",
                "│║══╬│",
                "│╬══║│",
                "┼────┼"
            }

        Dim numerOnePic() As String =
            {
                " ╔╗ ",
                "╔╝║ ",
                "╚╗║ ",
                " ║║ ",
                "╔╝╚╗",
                "╚══╝"
            }
        Dim numerTwoPic() As String =
            {
                "╔═══╗",
                "║╔═╗║",
                "╚╝╔╝║",
                "╔═╝╔╝",
                "║ ╚═╗",
                "╚═══╝"
            }
        Dim numerThreePic() As String =
            {
                "╔═══╗",
                "║╔═╗║",
                "╚╝╔╝║",
                "╔╗╚╗║",
                "║╚═╝║",
                "╚═══╝"
            }
        Dim userAns, str As String

        Dim pass As Boolean

        Dim second_chapter, fird_chapter As Boolean

        For i = 0 To UBound(doneLevelsFromCharpet1)
            If doneLevelsFromCharpet1(i) = False Then
                doneChapters(0) = False
            End If
        Next
        For i = 0 To UBound(doneLevelsFromCharpet2)
            If doneLevelsFromCharpet2(i) = False Then
                doneChapters(1) = False
            End If
        Next
        For i = 0 To UBound(doneLevelsFromCharpet3)
            If doneLevelsFromCharpet3(i) = False Then
                doneChapters(2) = False
            End If
        Next

        For i = 0 To UBound(doneLevelsFromCharpet1)
            If doneLevelsFromCharpet1(i) = False Then
                second_chapter = True
                fird_chapter = True
                Exit For
            Else
                second_chapter = False
            End If
        Next
        For i = 0 To UBound(doneLevelsFromCharpet2)
            If doneLevelsFromCharpet2(i) = False Then
                fird_chapter = True
                Exit For
            Else
                fird_chapter = False
            End If
        Next

        Do
            'Вывод трёх ворот
            Console.ForegroundColor = ConsoleColor.DarkGray
            displayingPictureFromSide(gates1, 20, 10, speed)
            displayingPictureFromSide(gates2, 56, 10, speed)
            displayingPictureFromSide(gates3, 92, 10, speed)
            Console.ResetColor()

            'Линия сквозь двери
            '[1]
            For i = 3 To 19
                displayingPictureFromSide(pattern, i, 22, speed + 0.00023)
            Next
            'Закрашиваем центр главы 1
            Console.ForegroundColor = ConsoleColor.Cyan
            displayingPictureFromSide(pattern2, 31, 21, speed)
            Console.ResetColor()
            'Закрашиваем цифру (1)
            Console.ForegroundColor = ConsoleColor.Cyan
            displayingPictureFromSide(numerOnePic, 25, 14, speed)
            Console.ResetColor()

            '[2]
            For i = 48 To 55
                displayingPictureFromSide(pattern, i, 22, speed + 0.0003)
            Next
            If second_chapter Then
                Console.ForegroundColor = ConsoleColor.DarkMagenta
            Else
                Console.ForegroundColor = ConsoleColor.Cyan
            End If
            'Закрашиваем центр главы 2
            displayingPictureFromSide(pattern2, 67, 21, speed)
            'Закрашиваем цифру (2)
            displayingPictureFromSide(numerTwoPic, 60, 14, speed)
            Console.ResetColor()

            '[3]
            For i = 84 To 91
                displayingPictureFromSide(pattern, i, 22, speed + 0.0003)
            Next
            If fird_chapter Then
                Console.ForegroundColor = ConsoleColor.DarkMagenta
            Else
                Console.ForegroundColor = ConsoleColor.Cyan
            End If
            'Закрашиваем центр главы 3
            displayingPictureFromSide(pattern2, 103, 21, speed)
            'Закрашиваем цифру (3)
            displayingPictureFromSide(numerThreePic, 96, 14, speed)
            Console.ResetColor()

            '[4]
            For i = 120 To 138
                displayingPictureFromSide(pattern, i, 22, speed + 0.00023)
            Next

            'Вывод chapter'ов
            Console.SetCursorPosition(0, 2)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            imageOutputLineByLine(chapter3Pic, speed + 0.07, 18)
            Console.ResetColor()

            Console.SetCursorPosition(19, 38)
            Console.ForegroundColor = ConsoleColor.Yellow
            Console.WriteLine("[ ENTER ""0"" (ZERO) TO RETURN ]")

            Console.SetCursorPosition(95, 38)
            Console.WriteLine("[ SELECT CHAPTER 1-3 ]")
            Console.ResetColor()

            '65, 38
            Console.SetCursorPosition(68, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            If second_chapter = False Then
                result = checkUserAns(userAns, 0, 2)
                If fird_chapter = False Then
                    result = checkUserAns(userAns, 0, 3)
                Else
                    result = checkUserAns(userAns, 0, 2)
                End If
            Else
                result = checkUserAns(userAns, 0, 1)
            End If

            If result Then
                pass = True
            Else
                speed = 0
                str = "[ INVALID VALUE ]"
                lineOutPutCharacterByCharacter(str, 61, 38, speed + 0.04, ConsoleColor.Red)
                Pause(1.7)
            End If
            Console.Clear()
        Loop Until pass

        Select Case userAns
            Case 0
                'Возвращение в меню выбора
                pathMenu()
            Case 1
                'Первая глава
                displayFisrtChapter()
            Case 2
                'Вторая глава
                display_second_chapter()
            Case 3
                'Третия глава
                display_fird_chapter()
        End Select

    End Sub
    Sub MainPlaceForLarsDialog()
        Dim firePlace() As String = {
            "▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄",
            "████████████████████████████████████████",
            " ██████████████████████████████████████ ",
            "  ▄▄   ▄▄▄▄▄▄▄   ▄▄▄▄▄▄   ▄▄▄▄▄▄▄   ▄▄  ",
            " ████ █████████ ████████ █████████ ████ ",
            " ▀▀▀▀ ▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀ ▀▀▀▀ ",
            " ▄██████▄ ▄██▀            ▀██▄ ▄██████▄ ",
            " ▀██████▀ █▀                ▀█ ▀██████▀ ",
            "  ▄▄   ▄▄                      ▄▄   ▄▄  ",
            " ████ ███                      ███ ████ ",
            " ▀▀▀▀ ▀▀▀                      ▀▀▀ ▀▀▀▀ ",
            " ▄██████▄                      ▄██████▄ ",
            " ▀██████▀                      ▀██████▀ ",
            " ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄ ",
            "████████████████████████████████████████",
            "████████████████████████████████████████"
                                    }

        Dim fire() As String =
            {
                "     █▄     ",
                "  ▄  ███▄   ",
                "  ██▄█████  ",
                " █████████ ▄",
                "██████ ▀████",
                "████▀   ████",
                "▄███▄▄▄████▄"
            }

        Dim larsName() As String =
            {
                "║               ║",
                "║               ║",
                "║               ║",
                "║               ║",
                "║               ║",
                "║               ║",
                "║               ║",
                "╔╗──╔══╗╔═══╗╔══╗",
                "║║──║╔╗║║╔═╗║║╔═╝",
                "║║──║╚╝║║╚═╝║║╚═╗",
                "║║──║╔╗║║╔╗╔╝╚═╗║",
                "║╚═╗║║║║║║║║─╔═╝║",
                "╚══╝╚╝╚╝╚╝╚╝─╚══╝"
            }

        Dim str As String

        'Вывод таблички |LARS|
        Console.ForegroundColor = ConsoleColor.DarkCyan
        ImageOutputLineByLine(larsName, speed, 110)
        Console.ResetColor()
        'Вывод изображения камина
        Console.ForegroundColor = ConsoleColor.Magenta
        DisplayingPictureFromSide(firePlace, 50, 20, speed - 0.003)
        Console.ResetColor()
        'Вывод изображения огня | В камине
        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(fire, 64, 26, speed)
        Console.ResetColor()

        'Вывод иконки персонажа
        str = "iconlars"
        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(IssuingAnIcon(str), 26, 10, speed)
        Console.ResetColor()

        'Вывод блока реплики
        str = "simpleDialogWindow"
        DisplayingPictureFromSide(DisplayingDialogBox(str), 41, 10, speed - 0.002)

        'Вывод |LARS|
        str = "LARS"
        LineOutPutCharacterByCharacter(str, 44, 11, speed, ConsoleColor.DarkYellow)
    End Sub
    Sub YesOrNoLars(ByRef userAns As String)
        Dim yesPic() As String =
            {
                "╔╗╔╗╔═══╗╔══╗",
                "║║║║║╔══╝║╔═╝",
                "║╚╝║║╚══╗║╚═╗",
                "╚═╗║║╔══╝╚═╗║",
                "─╔╝║║╚══╗╔═╝║",
                "─╚═╝╚═══╝╚══╝"
            }

        Dim noPic() As String =
            {
                "╔╗─╔╗ ╔══╗",
                "║╚═╝║ ║╔╗║",
                "║╔╗─║ ║║║║",
                "║║╚╗║ ║║║║",
                "║║─║║ ║╚╝║",
                "╚╝─╚╝ ╚══╝"
            }

        Dim str As String

        Dim pass As Boolean

        If kingdom_of_snails Then
            speed = 0
        End If

        Do
            MainPlaceForLarsDialog()

            'ВЫВОД ПРИВЕТСТВИЯ
            str = "ALWAYS GLAD TO SEE YOU ... CAN I HELP ANYTHING?"
            LineOutPutCharacterByCharacter(str, 44, 13, speed + 0.035, ConsoleColor.White)

            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(yesPic, 18, 26, speed)
            DisplayingPictureFromSide(noPic, 109, 26, speed)
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.SetCursorPosition(16, 26)
            Console.WriteLine("1")
            Console.SetCursorPosition(107, 26)
            Console.WriteLine("2")
            Console.ResetColor()

            Console.SetCursorPosition(56, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(userAns, 1, 2)

            If result Then
                pass = True
            Else
                'Вывод |Yes/No| в красном формате
                Console.ForegroundColor = ConsoleColor.Red
                DisplayingPictureFromSide(yesPic, 18, 26, speed)
                DisplayingPictureFromSide(noPic, 109, 26, speed)
                Console.ResetColor()
                Pause(1.2)
            End If
            speed = 0
            Console.Clear()
        Loop Until pass
    End Sub
    Sub ActionLars(ByRef userAns As String)
        Dim menuItemsFromLars() As String =
            {
                "BUY SODIER",
                "QUIET HOUR",
                "STORY",
                "NOTHING"
            }

        Dim infoItems() As String =
            {
                "[ ENTER NUMBER 1-3 TO GO TO THE CORRECT ITEMS ]",
                "[ ENTER ""4"" TO EXIT THE DIALOGUE ]"
            }

        Dim str As String

        Dim x, y, l As Integer

        Dim pass As Boolean

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.0035
        End If

        Do
            Console.Clear()
            MainPlaceForLarsDialog()
            'Вывод меню Ларса
            For i = 0 To UBound(menuItemsFromLars)
                Select Case i
                    Case 0
                        x = 48
                        y = 13
                    Case 1
                        x = 48
                        y = 15
                    Case 2
                        x = 70
                        y = 13
                    Case 3
                        x = 70
                        y = 15
                End Select
                Console.SetCursorPosition(x, y)
                Console.Write("{0}|", i + 1)
                str = menuItemsFromLars(i)
                LineOutPutCharacterByCharacter(str, x + 2, y, speed + 0.04, ConsoleColor.White)
            Next

            l = 3
            For i = 0 To UBound(infoItems)
                str = infoItems(i)
                If kingdom_of_snails Then
                    LineOutPutCharacterByCharacter(str, 42, l, 0, ConsoleColor.Yellow)
                Else
                    LineOutPutCharacterByCharacter(str, 42, l, 0.015, ConsoleColor.Yellow)
                End If

                l += 2
            Next

            Console.SetCursorPosition(56, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(userAns, 1, 4)

            If result Then
                pass = True
            Else
                speed = 0
            End If
        Loop Until pass
    End Sub
    Sub Null(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 141
        Console.SetBufferSize(141, 40)
        Console.Clear()

        Dim bigNullPic() As String =
            {
                "███╗  ██╗██╗   ██╗██╗     ██╗     ",
                "████╗ ██║██║   ██║██║     ██║     ",
                "██╔██╗██║██║   ██║██║     ██║     ",
                "██║╚████║██║   ██║██║     ██║     ",
                "██║ ╚███║╚██████╔╝███████╗███████╗",
                "╚═╝  ╚══╝ ╚═════╝ ╚══════╝╚══════╝"
            }

        Dim bigText() As String =
            {
                "[ NULL IN DATABASE MANAGEMENT SYSTEMS IS A SPECIAL",
                "VALUE THAT CAN BE WRITTEN TO A FIELD IN A DATABASE TABLE ]",
                "[ NULL CORRESPONDS TO THE CONCEPT OF AN ""EMPTY FIELD"",",
                "THAT IS, ""A FIELD THAT DOES Not CONTAIN ANY VALUE"" ]",
                "[ INTRODUCED IN ORDER TO DISTINGUISH BETWEEN EMPTY",
                "VALUES ​​AND MISSING VALUES ​​IN DATABASE FIELDS ]",
                "══════════════════════════════════════════════════════════",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        Console.WriteLine(Chr(10) & Chr(10) & Chr(10) & Chr(10))
        Console.ForegroundColor = ConsoleColor.Magenta
        ImageOutputLineByLine(bigNullPic, speed + 0.02, 94)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Blue
        ImageOutputLineByLine(bigNullPic, speed + 0.02, 94)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Yellow
        ImageOutputLineByLine(bigNullPic, speed + 0.02, 94)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Yellow
        ImageOutputLineByLine(bigNullPic, speed + 0.02, 94)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Yellow
        ImageOutputLineByLine(bigNullPic, speed + 0.02, 94)
        Console.ResetColor()

        'Вывод текста
        l = 12
        For i = 0 To UBound(bigText)
            str = bigText(i)
            If i = UBound(bigText) - 1 Then
                LineOutPutCharacterByCharacter(str, 20, l + 2, speed, ConsoleColor.DarkCyan)
            ElseIf i = UBound(bigText) Then
                LineOutPutCharacterByCharacter(str, 20, l + 4, speed, ConsoleColor.DarkYellow)
            Else
                LineOutPutCharacterByCharacter(str, 20, l, speed, ConsoleColor.DarkCyan)
            End If
            l += 2
        Next

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If
    End Sub
    Sub BlueScarf(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim bigBlueScarf() As String =
            {
                "╬║║║║║║║║║║║╬  ",
                "║════│││════║  ",
                "║════│││════║  ",
                "║════│││════║  ",
                "║════│││════║  ",
                "║════│││════║  ",
                "║════│││════║  ",
                "║════│││════║  ",
                "║╬═══════════╬ ",
                " ║════│││════║ ",
                " ║════│││════║ ",
                " ║════│││════║ ",
                " ║════│││════║ ",
                " ║════│││════║ ",
                " ║════│││════║ ",
                " ║╬══════════║╬",
                "  ║════│││════║",
                "  ║════│││════║",
                "  ║════│││════║",
                "  ║════│││════║",
                "  ║════│││════║",
                "  ╬║║║║║║║║║║║╬"
            }

        Dim bluePic() As String =
            {
                "║                ║",
                "║                ║",
                "║                ║",
                "║                ║",
                "║                ║",
                "║                ║",
                "╔══╗─╔╗──╔╗╔╗╔═══╗",
                "║╔╗║─║║──║║║║║╔══╝",
                "║╚╝╚╗║║──║║║║║╚══╗",
                "║╔═╗║║║──║║║║║╔══╝",
                "║╚═╝║║╚═╗║╚╝║║╚══╗",
                "╚═══╝╚══╝╚══╝╚═══╝"
            }

        Dim scarfPic() As String =
            {
                "║                   ║",
                "║                   ║",
                "║                   ║",
                "║                   ║",
                "║                   ║",
                "║                   ║",
                "╔══╗╔══╗╔══╗╔═══╗╔══╗",
                "║╔═╝║╔═╝║╔╗║║╔═╗║║╔═╝",
                "║╚═╗║║──║╚╝║║╚═╝║║╚═╗",
                "╚═╗║║║──║╔╗║║╔╗╔╝║╔═╝",
                "╔═╝║║╚═╗║║║║║║║║─║║  ",
                "╚══╝╚══╝╚╝╚╝╚╝╚╝─╚╝  "
            }

        Dim infoBlueScarf() As String =
            {
                "B L U E  S C A R F",
                "HE BLUE SCARF IS AN ITEM OF ARMOR.",
                "THIS TYPE OF ARMOR IS EFFECTIVE DURING SHORT BATTLES.",
                "ONE OF THE KEY TASKS OF THIS SCARF IS TO SCARE AWAY THE ENEMY.",
                "THE APPEARANCE IS NOT PLEASANT, WE WARN YOU IN ADVANCE",
                "[ HP + 20 ]",
                "[ ATTACK + 27 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод  изображения голубого шарфа
        Console.WriteLine(Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10))
        Console.ForegroundColor = ConsoleColor.Blue
        ImageOutputLineByLine(bigBlueScarf, speed + 0.02, 96)
        'Вывод надписи |Blue Scarf|
        Console.ForegroundColor = ConsoleColor.Magenta
        DisplayingPictureFromSide(bluePic, 30, 0, speed - 0.001)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Magenta
        DisplayingPictureFromSide(scarfPic, 54, 0, speed - 0.001)
        Console.ResetColor()
        'Вывод информации про шарф
        l = 15
        For i = 0 To UBound(infoBlueScarf)
            str = infoBlueScarf(i)
            If i = 0 Or i = UBound(infoBlueScarf) - 1 Or i = UBound(infoBlueScarf) - 2 Then
                LineOutPutCharacterByCharacter(str, 30, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(infoBlueScarf) Then
                LineOutPutCharacterByCharacter(str, 30, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 30, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        'Вывод |EPIC ITEM|
        Console.SetCursorPosition(50, 15)
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write("[ E P I C  I T E M ]")
        Console.ResetColor()

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If

    End Sub
    Sub ChocolateMarioMashroom(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim chocMarMush() As String =
            {
                "              ▄▄▄▄████▄▄▄▄▄             ",
                "         ▄▄██████▀▀▀▀▀▀██████▄▄         ",
                "       ▄██████▀          ▀██████▄       ",
                "     ▄██████▀              ▀██████▄     ",
                "   ▄███████                 ▀███████    ",
                "  ▄█▀ ▀████                  ████▀ ▀█   ",
                " ▄█    ████                  ████    █  ",
                " █     ████                  ████▄    █ ",
                "▄█    █████▄                ▄█████    █▄",
                "█    ▄███████             ▄███████▄    █",
                "█   ▄██████████▄▄      ▄▄██████████    █",
                "▀█▄▄████████████████████████████████▄ █▀",
                " ██████████████████████████████████████ ",
                " ▀██████████▀▀▀▀        ▀▀▀███████████  ",
                "   ██████▀     █▄      ▄█    ▀██████▀   ",
                "     ▀███      ██      ██      ███▀     ",
                "        █      ██      ██      █        ",
                "        ▀█     ▀        ▀     █▀        ",
                "         ▀█▄▄              ▄▄█▀         ",
                "            ▀▀▄▄▄▄▄▄▄▄▄▄▄▄▀▀            "
            }

        Dim chocMarMushWordsPic() As String =
            {
                "║                                     ║",
                "║                                     ║",
                "║                                     ║",
                "║                                     ║",
                "╔══╗╔╗╔╗╔══╗╔══╗╔══╗╔╗──╔══╗╔════╗╔═══╗",
                "║╔═╝║║║║║╔╗║║╔═╝║╔╗║║║──║╔╗║╚═╗╔═╝║╔══╝",
                "║║──║╚╝║║║║║║║──║║║║║║──║╚╝║──║║──║╚══╗",
                "║║──║╔╗║║║║║║║──║║║║║║──║╔╗║──║║──║╔══╝",
                "║╚═╗║║║║║╚╝║║╚═╗║╚╝║║╚═╗║║║║──║║──║╚══╗",
                "╚══╝╚╝╚╝╚══╝╚══╝╚══╝╚══╝╚╝╚╝──╚╝──╚═══╝",
                "║       ║                     ║       ║",
                "║       ╔╗──╔╗╔══╗╔═══╗╔══╗╔══╗       ║",
                "║       ║║──║║║╔╗║║╔═╗║╚╗╔╝║╔╗║       ║",
                "║       ║╚╗╔╝║║╚╝║║╚═╝║─║║─║║║║       ║",
                "║       ║╔╗╔╗║║╔╗║║╔╗╔╝─║║─║║║║       ║",
                "║       ║║╚╝║║║║║║║║║║─╔╝╚╗║╚╝║       ║",
                "║       ╚╝──╚╝╚╝╚╝╚╝╚╝─╚══╝╚══╝       ║",
                "║       ║                     ║       ║",
                " ╔╗──╔╗╔╗╔╗╔══╗╔╗╔╗╔═══╗╔══╗╔══╗╔╗──╔╗ ",
                " ║║──║║║║║║║╔═╝║║║║║╔═╗║║╔╗║║╔╗║║║──║║ ",
                " ║╚╗╔╝║║║║║║╚═╗║╚╝║║╚═╝║║║║║║║║║║╚╗╔╝║ ",
                " ║╔╗╔╗║║║║║╚═╗║║╔╗║║╔╗╔╝║║║║║║║║║╔╗╔╗║ ",
                " ║║╚╝║║║╚╝║╔═╝║║║║║║║║║─║╚╝║║╚╝║║║╚╝║║ ",
                " ╚╝──╚╝╚══╝╚══╝╚╝╚╝╚╝╚╝─╚══╝╚══╝╚╝──╚╝ "
            }

        Dim infoMarMush() As String =
            {
                "C H O C O L A T E  M A R I O  M U S H R O O M",
                "CHOCOLATE MARIO MUSHROOM IS A WAY TO RAISE MOOD AND RESTORE YOUR HEALTH!",
                "RARE ENOUGH THING, PRODUCED ONLY BY SPECIALLY PREPARED DRUIDES.",
                "WE ADVISE YOU TO HANDLE THIS ITEM, MANY ARE READY TO GIVE A LOT FOR IT. ",
                "IN SEE THIS IS A REGULAR CHOCOLATE MUSHROOM, A LITTLE WRINKED ON THE SIDE.",
                "[ MAX PLAYER HP + 30 ]",
                "[ CHILDREN'S HAPPINESS HERO ++; ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод изображения гриба
        Console.WriteLine(Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10))
        Console.ForegroundColor = ConsoleColor.DarkYellow
        ImageOutputLineByLine(chocMarMush, speed + 0.02, 86)
        'Вывод надписи |Chocolate Mario Mushroom|
        Console.ForegroundColor = ConsoleColor.DarkCyan
        DisplayingPictureFromSide(chocMarMushWordsPic, 1, 0, speed - 0.001)
        Console.ResetColor()
        'Вывод информации про газировку
        l = 4
        For i = 0 To UBound(infoMarMush)
            str = infoMarMush(i)
            If i = 0 Or i = UBound(infoMarMush) - 1 Or i = UBound(infoMarMush) - 2 Then
                LineOutPutCharacterByCharacter(str, 45, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(infoMarMush) Then
                LineOutPutCharacterByCharacter(str, 45, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 45, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        'Вывод |EPIC ITEM|
        Console.SetCursorPosition(92, 4)
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write("[ E P I C  I T E M ]")
        Console.ResetColor()

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If

    End Sub
    Sub CainMail(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim chainPic() As String =
            {
                "║                   ║",
                "║                   ║",
                "║                   ║",
                "║                   ║",
                "║                   ║",
                "║                   ║",
                "╔══╗╔╗╔╗╔══╗╔══╗╔╗─╔╗",
                "║╔═╝║║║║║╔╗║╚╗╔╝║╚═╝║",
                "║║──║╚╝║║╚╝║─║║─║╔╗─║",
                "║║──║╔╗║║╔╗║─║║─║║╚╗║",
                "║╚═╗║║║║║║║║╔╝╚╗║║─║║",
                "╚══╝╚╝╚╝╚╝╚╝╚══╝╚╝─╚╝"
            }

        Dim mailPic() As String =
            {
                "║              ║  ",
                "║              ║  ",
                "║              ║  ",
                "║              ║  ",
                "║              ║  ",
                "║              ║  ",
                "╔╗──╔╗╔══╗╔══╗╔╗  ",
                "║║──║║║╔╗║╚╗╔╝║║  ",
                "║╚╗╔╝║║╚╝║─║║─║║  ",
                "║╔╗╔╗║║╔╗║─║║─║║  ",
                "║║╚╝║║║║║║╔╝╚╗║╚═╗",
                "╚╝──╚╝╚╝╚╝╚══╝╚══╝"
            }

        Dim chainMailInfo() As String =
            {
                "C H A I N  M A I L",
                "THE CHAIN MAIL IS AN ELEMENT OF ARMOR INCREASING THE NUMBER OF THE PLAYER'S HP",
                "GOOD ENOUGH PRODUCT, MADE BY AN OLD WARDROBE.",
                "SERVED MANY WARRIORS WORTHY.",
                "HOPE YOU DON'T LETHE PREVIOUS OWNERS INVOLVED.",
                "[ HP + 25 ]",
                "[ ATTACK + 7 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод |chain mail| - pic
        Console.ForegroundColor = ConsoleColor.Magenta
        DisplayingPictureFromSide(chainPic, 39, 0, speed - 0.001)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Magenta
        DisplayingPictureFromSide(mailPic, 65, 0, speed - 0.001)
        Console.ResetColor()
        'Вывод информации про золотое яблоко
        l = 15
        For i = 0 To UBound(chainMailInfo)
            str = chainMailInfo(i)
            If i = 0 Or i = UBound(chainMailInfo) - 1 Or i = UBound(chainMailInfo) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(chainMailInfo) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If

    End Sub
    Sub GoldenApple(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim goldenPic() As String =
            {
                "║                          ║",
                "║                          ║",
                "║                          ║",
                "║                          ║",
                "║                          ║",
                "║                          ║",
                "╔═══╗╔══╗╔╗──╔══╗─╔═══╗╔╗─╔╗",
                "║╔══╝║╔╗║║║──║╔╗╚╗║╔══╝║╚═╝║",
                "║║╔═╗║║║║║║──║║╚╗║║╚══╗║╔╗─║",
                "║║╚╗║║║║║║║──║║─║║║╔══╝║║╚╗║",
                "║╚═╝║║╚╝║║╚═╗║╚═╝║║╚══╗║║─║║",
                "╚═══╝╚══╝╚══╝╚═══╝╚═══╝╚╝─╚╝"
            }

        Dim applePic() As String =
            {
                "║                     ║",
                "║                     ║",
                "║                     ║",
                "║                     ║",
                "║                     ║",
                "║                     ║",
                "╔══╗╔═══╗╔═══╗╔╗──╔═══╗",
                "║╔╗║║╔═╗║║╔═╗║║║──║╔══╝",
                "║╚╝║║╚═╝║║╚═╝║║║──║╚══╗",
                "║╔╗║║╔══╝║╔══╝║║──║╔══╝",
                "║║║║║║───║║───║╚═╗║╚══╗",
                "╚╝╚╝╚╝───╚╝───╚══╝╚═══╝"
            }

        Dim goldenAppleInfo() As String =
            {
                "G O L D E N  A P P L E",
                "THE GOLDEN APPLE IS AN ITEM OF POWER THAT POSSIBLE TO CONTROL. ",
                "GOLDEN APPLE RESTORES [30 HP] | REINFORCES ATTACK BY [20]. ",
                "IN SHAPE THIS IS AN ORDINARY APPLE, ONLY GOLD-COLORED. ",
                "TASTE DOESN'T DIFFER FROM CUCUMBER.",
                "[ HP + 30 ]",
                "[ ATTACK + 20 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод |Golden apple| - pic
        Console.ForegroundColor = ConsoleColor.Magenta
        DisplayingPictureFromSide(goldenPic, 39, 0, speed - 0.001)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Magenta
        DisplayingPictureFromSide(applePic, 70, 0, speed - 0.001)
        Console.ResetColor()
        'Вывод информации про золотое яблоко
        l = 15
        For i = 0 To UBound(goldenAppleInfo)
            str = goldenAppleInfo(i)
            If i = 0 Or i = UBound(goldenAppleInfo) - 1 Or i = UBound(goldenAppleInfo) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(goldenAppleInfo) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If

    End Sub
    Sub LambSweater(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim lambPic() As String =
            {
                "║                ║ ",
                "║                ║ ",
                "║                ║ ",
                "║                ║ ",
                "║                ║ ",
                "║                ║ ",
                "╔╗──╔══╗╔╗──╔╗╔══╗ ",
                "║║──║╔╗║║║──║║║╔╗║ ",
                "║║──║╚╝║║╚╗╔╝║║╚╝╚╗",
                "║║──║╔╗║║╔╗╔╗║║╔═╗║",
                "║╚═╗║║║║║║╚╝║║║╚═╝║",
                "╚══╝╚╝╚╝╚╝──╚╝╚═══╝"
            }

        Dim sweaterPic() As String =
            {
                "║                                 ║",
                "║                                 ║",
                "║                                 ║",
                "║                                 ║",
                "║                                 ║",
                "║                                 ║",
                "╔══╗╔╗╔╗╔╗╔═══╗╔══╗╔════╗╔═══╗╔═══╗",
                "║╔═╝║║║║║║║╔══╝║╔╗║╚═╗╔═╝║╔══╝║╔═╗║",
                "║╚═╗║║║║║║║╚══╗║╚╝║──║║──║╚══╗║╚═╝║",
                "╚═╗║║║║║║║║╔══╝║╔╗║──║║──║╔══╝║╔╗╔╝",
                "╔═╝║║╚╝╚╝║║╚══╗║║║║──║║──║╚══╗║║║║ ",
                "╚══╝╚═╝╚═╝╚═══╝╚╝╚╝──╚╝──╚═══╝╚╝╚╝ "
            }

        Dim lambSweaterInfo() As String =
            {
                "L A M B  S W E A T E R",
                "THE LAMB SWEATER IS A PIECE OF ARMOR THAT CAN PROTECT AGAINST LIGHT ATTACKS. ",
                "IT WAS NOT ORIGINALLY PLANNED AS A DEFENSE, BUT HAS PROVEN EFFECTIVE IN NUMEROUS BATTLES.",
                "THE MOST IMPORTANT STRENGTH IS THE AFFECTION OF THE ENEMY. ",
                "BUT BEWARE, SOMEONE MIGHT HIT YOU SERIOUSLY...",
                "[ HP + 14 ]",
                "[ ATTACK + 3 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод |Lamb Sweater|
        Console.ForegroundColor = ConsoleColor.DarkCyan
        DisplayingPictureFromSide(lambPic, 40, 0, speed - 0.001)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.DarkCyan
        DisplayingPictureFromSide(sweaterPic, 69, 0, speed - 0.001)
        Console.ResetColor()

        'Вывод информации про свитер
        l = 15
        For i = 0 To UBound(lambSweaterInfo)
            str = lambSweaterInfo(i)
            If i = 0 Or i = UBound(lambSweaterInfo) - 1 Or i = UBound(lambSweaterInfo) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(lambSweaterInfo) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If

    End Sub
    Sub GreenShorts(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim greenPic() As String =
            {
                "║                       ║",
                "║                       ║",
                "║                       ║",
                "║                       ║",
                "║                       ║",
                "║                       ║",
                "╔═══╗╔═══╗╔═══╗╔═══╗╔╗─╔╗",
                "║╔══╝║╔═╗║║╔══╝║╔══╝║╚═╝║",
                "║║╔═╗║╚═╝║║╚══╗║╚══╗║╔╗─║",
                "║║╚╗║║╔╗╔╝║╔══╝║╔══╝║║╚╗║",
                "║╚═╝║║║║║─║╚══╗║╚══╗║║─║║",
                "╚═══╝╚╝╚╝─╚═══╝╚═══╝╚╝─╚╝"
            }

        Dim shortsPic() As String =
            {
                "║                         ║",
                "║                         ║",
                "║                         ║",
                "║                         ║",
                "║                         ║",
                "║                         ║",
                "╔══╗╔╗╔╗╔══╗╔═══╗╔════╗╔══╗",
                "║╔═╝║║║║║╔╗║║╔═╗║╚═╗╔═╝║╔═╝",
                "║╚═╗║╚╝║║║║║║╚═╝║──║║──║╚═╗",
                "╚═╗║║╔╗║║║║║║╔╗╔╝──║║──╚═╗║",
                "╔═╝║║║║║║╚╝║║║║║───║║──╔═╝║",
                "╚══╝╚╝╚╝╚══╝╚╝╚╝───╚╝──╚══╝"
            }

        Dim greenShortsInfo() As String =
            {
                "G R E E N  S H O R T S",
                "BEAUTIFUL GREEN SHORTS!",
                "SUITABLE FOR STREET AND FOR HOME.",
                "COMPACT AND STYLISH - YOUTH LOVE.",
                "[ HP + 7 ]",
                "[ ATTACK + 5 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод |Green Shorts|
        Console.ForegroundColor = ConsoleColor.Green
        DisplayingPictureFromSide(greenPic, 39, 0, speed - 0.001)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Green
        DisplayingPictureFromSide(shortsPic, 70, 0, speed - 0.001)
        Console.ResetColor()
        'Вывод информации про шорты
        l = 15
        For i = 0 To UBound(greenShortsInfo)
            str = greenShortsInfo(i)
            If i = 0 Or i = UBound(greenShortsInfo) - 1 Or i = UBound(greenShortsInfo) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(greenShortsInfo) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If

    End Sub
    Sub WoodenSword(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim woodenPic() As String =
            {
                "║                           ║",
                "║                           ║",
                "║                           ║",
                "║                           ║",
                "║                           ║",
                "║                           ║",
                "╔╗╔╗╔╗╔══╗╔══╗╔══╗─╔═══╗╔╗─╔╗",
                "║║║║║║║╔╗║║╔╗║║╔╗╚╗║╔══╝║╚═╝║",
                "║║║║║║║║║║║║║║║║╚╗║║╚══╗║╔╗─║",
                "║║║║║║║║║║║║║║║║─║║║╔══╝║║╚╗║",
                "║╚╝╚╝║║╚╝║║╚╝║║╚═╝║║╚══╗║║─║║",
                "╚═╝╚═╝╚══╝╚══╝╚═══╝╚═══╝╚╝─╚╝"
            }

        Dim swordPic() As String =
            {
                "║                     ║ ",
                "║                     ║ ",
                "║                     ║ ",
                "║                     ║ ",
                "║                     ║ ",
                "║                     ║ ",
                "╔══╗╔╗╔╗╔╗╔══╗╔═══╗╔══╗ ",
                "║╔═╝║║║║║║║╔╗║║╔═╗║║╔╗╚╗",
                "║╚═╗║║║║║║║║║║║╚═╝║║║╚╗║",
                "╚═╗║║║║║║║║║║║║╔╗╔╝║║─║║",
                "╔═╝║║╚╝╚╝║║╚╝║║║║║─║╚═╝║",
                "╚══╝╚═╝╚═╝╚══╝╚╝╚╝─╚═══╝"
            }

        Dim woodenSwordInfo() As String =
            {
                "W O O D E N  S W O R D",
                "THE WOODEN SWORD IS THE FIRST-CLASS WEAPON OF A REAL WARRIOR!",
                "MADE FROM RARE WOOD: MELONIA.",
                "REMINDS YOU OF CHILDHOOD.",
                "[ HP + 3 ]",
                "[ ATTACK + 21 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод |Wooden Sword|
        Console.ForegroundColor = ConsoleColor.DarkGray
        DisplayingPictureFromSide(woodenPic, 39, 0, speed - 0.001)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.DarkGray
        DisplayingPictureFromSide(swordPic, 70, 0, speed - 0.001)
        Console.ResetColor()
        'Вывод информации про меч
        l = 15
        For i = 0 To UBound(woodenSwordInfo)
            str = woodenSwordInfo(i)
            If i = 0 Or i = UBound(woodenSwordInfo) - 1 Or i = UBound(woodenSwordInfo) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(woodenSwordInfo) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If
    End Sub
    Sub Binoculars(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim binocularsPic() As String =
            {
                "║                                         ║",
                "║                                         ║",
                "║                                         ║",
                "║                                         ║",
                "║                                         ║",
                "║                                         ║",
                "╔══╗─╔══╗╔╗─╔╗╔══╗╔══╗╔╗╔╗╔╗──╔══╗╔═══╗╔══╗",
                "║╔╗║─╚╗╔╝║╚═╝║║╔╗║║╔═╝║║║║║║──║╔╗║║╔═╗║║╔═╝",
                "║╚╝╚╗─║║─║╔╗─║║║║║║║──║║║║║║──║╚╝║║╚═╝║║╚═╗",
                "║╔═╗║─║║─║║╚╗║║║║║║║──║║║║║║──║╔╗║║╔╗╔╝╚═╗║",
                "║╚═╝║╔╝╚╗║║─║║║╚╝║║╚═╗║╚╝║║╚═╗║║║║║║║║─╔═╝║",
                "╚═══╝╚══╝╚╝─╚╝╚══╝╚══╝╚══╝╚══╝╚╝╚╝╚╝╚╝─╚══╝"
            }

        Dim binocularsInfo() As String =
            {
                "B I N O C U L A R S",
                "BINOCULARS ARE THE WEAPON OF A TRUE FOUNDER AND HUNTER.",
                "THANKS TO THIS THING, YOU CAN UNDERSTAND AN OPPONENT'S STRATEGY",
                "APPEARANCE, BUT I HAVE NOT LOST ITS QUALITY",
                "[ HP + 10 ]",
                "[ ATTACK + 19 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод |Binoculars|
        Console.ForegroundColor = ConsoleColor.Blue
        DisplayingPictureFromSide(binocularsPic, 37, 0, speed - 0.001)
        Console.ResetColor()
        'Вывод информации про бинокль
        l = 15
        For i = 0 To UBound(binocularsInfo)
            str = binocularsInfo(i)
            If i = 0 Or i = UBound(binocularsInfo) - 1 Or i = UBound(binocularsInfo) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(binocularsInfo) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If
    End Sub
    Sub IronSword(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim ironPic() As String =
            {
                "║                ║",
                "║                ║",
                "║                ║",
                "║                ║",
                "║                ║",
                "║                ║",
                "╔══╗╔═══╗╔══╗╔╗─╔╗",
                "╚╗╔╝║╔═╗║║╔╗║║╚═╝║",
                "─║║─║╚═╝║║║║║║╔╗─║",
                "─║║─║╔╗╔╝║║║║║║╚╗║",
                "╔╝╚╗║║║║─║╚╝║║║─║║",
                "╚══╝╚╝╚╝─╚══╝╚╝─╚╝"
            }

        Dim swordPic() As String =
            {
                "║                     ║ ",
                "║                     ║ ",
                "║                     ║ ",
                "║                     ║ ",
                "║                     ║ ",
                "║                     ║ ",
                "╔══╗╔╗╔╗╔╗╔══╗╔═══╗╔══╗ ",
                "║╔═╝║║║║║║║╔╗║║╔═╗║║╔╗╚╗",
                "║╚═╗║║║║║║║║║║║╚═╝║║║╚╗║",
                "╚═╗║║║║║║║║║║║║╔╗╔╝║║─║║",
                "╔═╝║║╚╝╚╝║║╚╝║║║║║─║╚═╝║",
                "╚══╝╚═╝╚═╝╚══╝╚╝╚╝─╚═══╝"
            }

        Dim ironSwordInfo() As String =
            {
                "I R O N  S W O R D",
                "THE IRON SWORD IS A REALLY DREAMING WEAPON.",
                "HE IS FORGED BY THE BEST FORGE IN OUR VILLAGE.",
                "TAKE IT I CRUSH YOUR ENEMIES.",
                "[ HP + 10 ]",
                "[ ATTACK + 37 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод |Iron Sword|
        Console.ForegroundColor = ConsoleColor.White
        DisplayingPictureFromSide(ironPic, 41, 0, speed - 0.001)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.White
        DisplayingPictureFromSide(swordPic, 68, 0, speed - 0.001)
        Console.ResetColor()
        'Вывод информации о мече
        l = 15
        For i = 0 To UBound(ironSwordInfo)
            str = ironSwordInfo(i)
            If i = 0 Or i = UBound(ironSwordInfo) - 1 Or i = UBound(ironSwordInfo) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(ironSwordInfo) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If

    End Sub
    Sub DruidStaff(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim druidsPic() As String =
            {
                "║                           ║",
                "║                           ║",
                "║                           ║",
                "║                           ║",
                "║                           ║",
                "║                           ║",
                "╔══╗─╔═══╗╔╗╔╗╔══╗╔══╗─╔╗╔══╗",
                "║╔╗╚╗║╔═╗║║║║║╚╗╔╝║╔╗╚╗╚╝║╔═╝",
                "║║╚╗║║╚═╝║║║║║─║║─║║╚╗║──║╚═╗",
                "║║─║║║╔╗╔╝║║║║─║║─║║─║║──╚═╗║",
                "║╚═╝║║║║║─║╚╝║╔╝╚╗║╚═╝║──╔═╝║",
                "╚═══╝╚╝╚╝─╚══╝╚══╝╚═══╝──╚══╝"
            }

        Dim staffPic() As String =
            {
                "║                    ║",
                "║                    ║",
                "║                    ║",
                "║                    ║",
                "║                    ║",
                "║                    ║",
                "╔══╗╔════╗╔══╗╔══╗╔══╗",
                "║╔═╝╚═╗╔═╝║╔╗║║╔═╝║╔═╝",
                "║╚═╗──║║──║╚╝║║╚═╗║╚═╗",
                "╚═╗║──║║──║╔╗║║╔═╝║╔═╝",
                "╔═╝║──║║──║║║║║║──║║  ",
                "╚══╝──╚╝──╚╝╚╝╚╝──╚╝  "
            }

        Dim staffRealPic() As String =
            {
                "       ┼      ",
                "              ",
                "       ╬    ┼ ",
                "      ╬║═     ",
                "┼     ═║═╬    ",
                "      ╬║     ",
                " ┼     ║   ┼  ",
                "       ║║     ",
                "       ║      ",
                "       ║║     ",
                "       ║║     ",
                "      ║║║     ",
                "      ═╬═     ",
                "      ║║      ",
                "      ║║      ",
                "      ║║      ",
                "       ║      ",
                "       ║      ",
                "       ║      ",
                "       ║      ",
                "       ║      ",
                "       ║      ",
                "   ═════════  "
            }

        Dim druidsStaffInfo() As String =
            {
                "D R U I D ' S  S T A F F",
                "DRUID STAFF - UNIQUE DRUID PERSONAL WEAPON.",
                "IN THE MOST WEDDINGS OF THE FOREST, IN THE LIGHT OF THE FULL MOON,",
                "EVERY 125 YEARS A DRUID'S STAFF IS CREATED.",
                "[ HP + 60 ]",
                "[ ATTACK + 90 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод изображения посоха
        Console.WriteLine(Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10))
        Console.ForegroundColor = ConsoleColor.Magenta
        ImageOutputLineByLine(staffRealPic, speed + 0.02, 107)

        'Вывод надписи |Druid's Staff|
        Console.ForegroundColor = ConsoleColor.White
        DisplayingPictureFromSide(druidsPic, 37, 0, speed - 0.001)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.DarkCyan
        DisplayingPictureFromSide(staffPic, 72, 0, speed - 0.001)
        Console.ResetColor()

        'Вывод информации про посох
        l = 15
        For i = 0 To UBound(druidsStaffInfo)
            str = druidsStaffInfo(i)
            If i = 0 Or i = UBound(druidsStaffInfo) - 1 Or i = UBound(druidsStaffInfo) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(druidsStaffInfo) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        'Вывод |EPIC ITEM|
        Console.SetCursorPosition(63, 15)
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write("[ E P I C  I T E M ]")
        Console.ResetColor()

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If

    End Sub
    Sub BookOfTheDamned(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim bookPic() As String =
            {
                "            ░▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄░░░░        ",
                "           ░▄████████████████▄▄░        ",
                "          ░▄████████████████████░       ",
                "         ░▄█████████████████████▄░      ",
                "        ░▄███████████████████████▄░     ",
                "       ░░█████████████████████████░░    ",
                "       ░█████████████████████████▀░░    ",
                "      ░████████████████████████▀░░░░░   ",
                "    ░▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀░░░░░░░░   ",
                "   ░▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄░ ",
                "  ░▄█▀░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░   ",
                "  ░█▀░░░░░▄▄▄▄▄▄▄▄▄▄▄░░░░░░░░░░░░░░░    ",
                "  ░█░░░░░░▀█▀▀▀▀▀▀██▀░░░░░░░░░░░░░░░    ",
                "  ░▀█░░░░░░█░░░░░░██░░░░░░░░░░░░░░░░░   ",
                "   ░▀█▄▄▄▄██░░░░░░██▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄   ",
                "    ░░░░▄██░░░░░░▄█░░░░░░░░░░░░░░░░░░░░ ",
                "        ▀▀░░░░░░                        ",
                "        ░░░░░░                          "
            }

        Dim bookOfTheDammedWordsPic() As String =
            {
                "║                             ║             ║                             ║ ",
                "║                             ║             ║                             ║ ",
                "║                             ║             ║                             ║ ",
                "╔══╗─╔══╗╔══╗╔╗╔══╗──╔══╗╔══╗─╔════╗╔╗╔╗╔═══╗─╔══╗─╔══╗╔╗──╔╗╔╗─╔╗╔═══╗╔══╗ ",
                "║╔╗║─║╔╗║║╔╗║║║║╔═╝──║╔╗║║╔═╝─╚═╗╔═╝║║║║║╔══╝─║╔╗╚╗║╔╗║║║──║║║╚═╝║║╔══╝║╔╗╚╗",
                "║╚╝╚╗║║║║║║║║║╚╝║────║║║║║╚═╗───║║──║╚╝║║╚══╗─║║╚╗║║╚╝║║╚╗╔╝║║╔╗─║║╚══╗║║╚╗║",
                "║╔═╗║║║║║║║║║║╔╗║────║║║║║╔═╝───║║──║╔╗║║╔══╝─║║─║║║╔╗║║╔╗╔╗║║║╚╗║║╔══╝║║─║║",
                "║╚═╝║║╚╝║║╚╝║║║║╚═╗──║╚╝║║║─────║║──║║║║║╚══╗─║╚═╝║║║║║║║╚╝║║║║─║║║╚══╗║╚═╝║",
                "╚═══╝╚══╝╚══╝╚╝╚══╝──╚══╝╚╝─────╚╝──╚╝╚╝╚═══╝─╚═══╝╚╝╚╝╚╝──╚╝╚╝─╚╝╚═══╝╚═══╝"
            }

        Dim bookOTDInfo() As String =
            {
                "B O O K  O F  T H E  D A M N E D",
                "REAL EVIL HIDDEN IN THIS BOOK ...",
                "NOT MANY COULD FIGHT WITH IT. THIS IS A POWERFUL BOOK!",
                "TAKE IT AT YOUR OWN RISK",
                "[ CHARACTER HEALTH + 30 ]",
                "[ ATTACK = 372 ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод изображения книги
        Console.WriteLine(Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10))
        Console.ForegroundColor = ConsoleColor.DarkMagenta
        ImageOutputLineByLine(bookPic, speed + 0.02, 98)
        'Вывод |bookOfTheDammed|
        Console.ForegroundColor = ConsoleColor.Gray
        DisplayingPictureFromSide(bookOfTheDammedWordsPic, 37, 0, speed - 0.001)
        'Вывод информации о книге
        l = 15
        For i = 0 To UBound(bookOTDInfo)
            str = bookOTDInfo(i)
            If i = 0 Or i = UBound(bookOTDInfo) - 1 Or i = UBound(bookOTDInfo) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(bookOTDInfo) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        'Вывод |LEGENDARY ITEM|
        Console.SetCursorPosition(72, 15)
        Console.ForegroundColor = ConsoleColor.DarkRed
        Console.Write("[ L E G E N D A R Y  I T E M ]")
        Console.ResetColor()

        Console.ReadKey()

        If spNum = 1 Then
            Tavern()
        ElseIf spNum = 2 Then
            ironChest()
        End If

    End Sub
    Sub Gasing(spNum As Integer)
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0035

        Dim gazingPic() As String =
            {
                "    ▄██████▄    ",
                "    ██▄▄▄▄██    ",
                "    ▀█▀▀▀▀█▀    ",
                "    ▄█    █▄    ",
                "    ██    ██    ",
                "    ██    ██    ",
                "   ▄█▀    ▀█▄   ",
                " ▄█▀        ▀█▄ ",
                "▄█▀          ▀█▄",
                "██            ██",
                "██   ▄████▄   ██",
                "█████▀    ▀█████",
                "██ ██      ██ ██",
                " █ ██      ██ █ ",
                " █ ██      ██ █ ",
                "▄█▄██      ██▄█▄",
                "██▀▀▀█▄  ▄█▀▀▀██",
                "██    ▀▀▀▀    ██",
                "██            ██",
                "██            ██",
                "██            ██",
                " ▀████████████▀ "
            }

        Dim gasingWordPic() As String =
            {
                "║                              ║",
                "║                              ║",
                "║                              ║",
                "║                              ║",
                "║                              ║",
                "║                              ║",
                "╔═══╗ ╔══╗ ╔══╗ ╔══╗ ╔╗─╔╗ ╔═══╗",
                "║╔══╝ ║╔╗║ ║╔═╝ ╚╗╔╝ ║╚═╝║ ║╔══╝",
                "║║╔═╗ ║╚╝║ ║╚═╗ ─║║─ ║╔╗─║ ║║╔═╗",
                "║║╚╗║ ║╔╗║ ╚═╗║ ─║║─ ║║╚╗║ ║║╚╗║",
                "║╚═╝║ ║║║║ ╔═╝║ ╔╝╚╗ ║║─║║ ║╚═╝║",
                "╚═══╝ ╚╝╚╝ ╚══╝ ╚══╝ ╚╝─╚╝ ╚═══╝"
            }

        Dim infoGazing() As String =
            {
                "G A S I N G",
                "A PURPLE BOTTLE WITH ONE OF THE MOST POPULAR DRINKS",
                "OF ALL TIME.",
                "ENJOY EVERY SECOND TOGETHER WITH ""BLUE LAGOON SODA""",
                "POWER IN BUBBLES! WHO INVENTED THIS SLOGAN??",
                "[ HP++: 20 ]",
                "[ PRICE: 25 C ]",
                "[ PRESS ""ENTER"" TO RETURN ]"
            }

        Dim l As Integer

        Dim str As String

        'Вывод изображения бутылки
        Console.WriteLine(Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10))
        Console.ForegroundColor = ConsoleColor.Magenta
        ImageOutputLineByLine(gazingPic, speed + 0.02, 94)
        Console.ResetColor()
        'Вывод надписи |gasing|
        Console.ForegroundColor = ConsoleColor.Blue
        DisplayingPictureFromSide(gasingWordPic, 37, 0, speed - 0.001)
        Console.ResetColor()
        'Вывод информации про газировку
        l = 15
        For i = 0 To UBound(infoGazing)
            str = infoGazing(i)
            If i = 0 Or i = UBound(infoGazing) - 1 Or i = UBound(infoGazing) - 2 Then
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.DarkYellow)
            ElseIf i = UBound(infoGazing) Then
                LineOutPutCharacterByCharacter(str, 37, l + 3, speed, ConsoleColor.Yellow)
            Else
                LineOutPutCharacterByCharacter(str, 37, l, speed, ConsoleColor.Yellow)
            End If
            l += 2
        Next

        Console.ReadKey()
        If spNum = 0 Then
            soda()
        ElseIf spNum = 2 Then
            ironChest()
        Else
            Tavern()
        End If

    End Sub
    Sub QuietHour()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        Dim num1() As String = {
            " ╔╗ ",
            "╔╝║ ",
            "╚╗║ ",
            " ║║ ",
            " ║║ ",
            " ╚╝ "
                               }
        Dim num2() As String = {
            "╔══╗",
            "╚═╗║",
            "╔═╝║",
            "║╔═╝",
            "║╚═╗",
            "╚══╝"
                               }
        Dim num3() As String = {
            "╔══╗",
            "╚═╗║",
            "╔═╝║",
            "╚═╗║",
            "╔═╝║",
            "╚══╝"
                               }

        Dim num4() As String = {
            "╔╗╔╗",
            "║║║║",
            "║╚╝║",
            "╚═╗║",
            "  ║║",
            "  ╚╝"
                                }

        Dim num5() As String = {
            "╔══╗",
            "║╔═╝",
            "║╚═╗",
            "╚═╗║",
            "╔═╝║",
            "╚══╝"
                                }

        Dim Speech1() As String =
            {
                "SLEEP GROMMASH HELLSCREAM... FILL YOUR STRENGTH!",
                "HAHAHA I'M JOKING",
                "GOOD NIGHT"
            }

        Dim Speech2() As String =
            {
                "I WISH YOU GET EXCELLENT SLEEP!",
                "AND I'M GO FREESHING WITH A BOTTLE OF SODA.",
                "[ YOUR HEALTH IS FULFILLED TO THE LIMIT ]",
                "[ YOUR STAMINA IS FILLED TO THE LIMIT ]"
            }

        Dim Speech3() As String =
            {
                "YOUR ROOM IS READY!",
                "PLEASANT DREAMS...",
                "[ YOUR MOOD IS IMPROVED ]",
                "[ YOUR STAMINA IS FILLED TO THE LIMIT ]"
            }

        Dim str As String

        Dim rndNum As Integer

        Randomize()
        rndNum = Rnd() * (2 - 0) + 0 '0 - 2

        'Вывод иконки персонажа
        str = "iconlars"
        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(IssuingAnIcon(str), 26, 10, speed)
        Console.ResetColor()

        'Вывод блока реплики
        str = "simpleDialogWindow"
        DisplayingPictureFromSide(DisplayingDialogBox(str), 41, 10, speed - 0.002)

        'Вывод |LARS|
        str = "LARS"
        LineOutPutCharacterByCharacter(str, 44, 11, speed, ConsoleColor.DarkYellow)

        'Вывод речи
        For i = 0 To 2
            Console.SetCursorPosition(44, 13)
            Console.WriteLine(StrDup(53, " "))
            Select Case rndNum
                Case 0
                    str = Speech1(i)
                    LineOutPutCharacterByCharacter(str, 44, 13, speed + 0.05, ConsoleColor.White)
                Case 1
                    If i = 2 Then
                        str = Speech2(i)
                        LineOutPutCharacterByCharacter(str, 44, 13, speed + 0.05, ConsoleColor.White)
                        str = Speech2(i + 1)
                        LineOutPutCharacterByCharacter(str, 44, 15, speed + 0.05, ConsoleColor.White)
                    Else
                        str = Speech2(i)
                        LineOutPutCharacterByCharacter(str, 44, 13, speed + 0.05, ConsoleColor.White)
                    End If
                Case 2
                    If i = 2 Then
                        str = Speech3(i)
                        LineOutPutCharacterByCharacter(str, 44, 13, speed + 0.05, ConsoleColor.White)
                        str = Speech3(i + 1)
                        LineOutPutCharacterByCharacter(str, 44, 15, speed + 0.05, ConsoleColor.White)
                    Else
                        str = Speech3(i)
                        LineOutPutCharacterByCharacter(str, 44, 13, speed + 0.05, ConsoleColor.White)
                    End If
            End Select
            Pause(1)
        Next

        'Восполнение stamina
        playerStamina = 14

        'Отсчёт от 5 до 1
        Console.ForegroundColor = ConsoleColor.Cyan
        For i = 1 To 5
            Select Case i
                Case 1
                    DisplayingPictureFromSide(num5, 103, 11, speed)
                Case 2
                    DisplayingPictureFromSide(num4, 103, 11, speed)
                Case 3
                    DisplayingPictureFromSide(num3, 103, 11, speed)
                Case 4
                    DisplayingPictureFromSide(num2, 103, 11, speed)
                Case 5
                    DisplayingPictureFromSide(num1, 103, 11, speed)
            End Select
            Pause(1)
        Next
        Console.ResetColor()
        Tavern()
    End Sub
    Sub Soda()
        Console.Clear()

        Dim infoItems() As String =
            {
                "[ ENTER THE NUMBER OF THE PRODUCT YOU WANT TO PURCHASE ]",
                "[ ENTER ""0""(ZERO) TO RETURN ]",
                "[ ENTER THE PRICE OF THE PRODUCT (25)",
                "  TO READ THE INFORMATION ABOUT THIS PRODUCT ]"
            }

        Dim str, userAns As String

        Dim Counter, l As Integer

        Dim pass As Boolean

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.0035
        End If


        Do
            MainPlaceForLarsDialog()
            'Покупка содовой
            str = "SODA! YOU KNOW WHO TO ASK"
            LineOutPutCharacterByCharacter(str, 44, 13, speed + 0.04, ConsoleColor.White)
            Pause(0.7)

            'Вывод информации о вещах игрока
            Console.SetCursorPosition(102, 26)
            Console.Write("SODA STOCK: ")
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write("{0}", playerSoda)
            Console.ResetColor()

            Console.SetCursorPosition(102, 28)
            Console.Write("FREE SPACE IN INVENTORY: ")
            Counter = maxPlayerBag - playerBag
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write("{0}", Counter)
            Console.ResetColor()

            Console.SetCursorPosition(102, 30)
            Console.Write("COINS: ")
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write("{0}", playerCoins)
            Console.ResetColor()

            Console.SetCursorPosition(14, 26)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.Write("| COST OF GASING |")
            Console.ResetColor()

            Console.SetCursorPosition(14, 28)
            Console.Write("1| GASING [BLUE LAGOON]: ")
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.Write("25 C")
            Console.ResetColor()

            'Вывод инормации
            l = 2
            For i = 0 To UBound(infoItems)
                str = infoItems(i)
                LineOutPutCharacterByCharacter(str, 42, l, speed + 0.01, ConsoleColor.Yellow)
                l += 2
            Next

            Console.SetCursorPosition(56, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            If userAns <> "25" And userAns <> "1" And userAns <> "0" Then
                pass = False
                speed = 0
                Console.Clear()
            Else
                pass = True
            End If
        Loop Until pass

        Select Case userAns
            Case 0
                'Возвращение назад
                LarsDialog()
            Case 1
                If playerCoins >= 25 And playerBag < 6 Then
                    playerCoins -= 25
                    playerBag += 1
                    For i = 0 To UBound(playerInventory)
                        If playerInventory(i) = False Then
                            playerInventory(i) = True
                            playerInventoryNames(i) = "GASING [BLUE LAGOON]"
                            Exit For
                        End If
                    Next
                    str = "[ PURCHASE COMPLETED SUCCESSFULLY ]"
                    LineOutPutCharacterByCharacter(str, 52, 38, speed, ConsoleColor.Green)
                    Console.ResetColor()
                Else
                    Console.SetCursorPosition(59, 38)
                    Console.SetCursorPosition(59, 38)
                    Console.ForegroundColor = ConsoleColor.Red
                    If playerCoins < 25 Then
                        If playerBag >= 6 Then
                            str = "[ NOT ENOUGH COINS TO BUY | INVENTORY IS FULL ]"
                            LineOutPutCharacterByCharacter(str, 59, 38, speed, ConsoleColor.Red)
                        Else
                            str = "[ NOT ENOUGH COINS TO BUY ]"
                            LineOutPutCharacterByCharacter(str, 59, 38, speed, ConsoleColor.Red)
                        End If
                    ElseIf playerBag >= 6 Then
                        str = "[ INVENTORY IS FULL ]"
                        LineOutPutCharacterByCharacter(str, 59, 38, speed, ConsoleColor.Red)
                    End If
                    Console.ResetColor()
                End If
                Pause(2)
                Soda()
            Case 25
                'Вывод информации про газировку
                Gasing(0)
        End Select

    End Sub
    Sub LarsDialog()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        If Not kingdom_of_snails Then
            speed = 0.0035
        Else
            speed = 0
        End If

        Dim str, userAns As String

        userAns = ""

        yesOrNoLars(userAns)

        If userAns = 1 Then
            actionLars(userAns)
        Else
            mainPlaceForLarsDialog()
            str = "AS YOU SAY. HAVE A NICE DAY!"
            lineOutPutCharacterByCharacter(str, 44, 13, speed + 0.04, ConsoleColor.White)
            Pause(2)
            Tavern()
        End If

        Console.Clear()
        Select Case userAns
            Case 1
                soda()
            Case 2
                quietHour()
            Case 3

            Case 4
                mainPlaceForLarsDialog()
                'Возвращение в Таверну
                str = "NOTHING SO NOTHING... QUESTIONS WILL APPEAR - ASK"
                lineOutPutCharacterByCharacter(str, 44, 13, speed + 0.04, ConsoleColor.White)
                Pause(2)

                check_tasks_for_complete()
                Tavern()
        End Select

    End Sub
    Sub PlayerItemsInfo(itemName As String)

        Select Case itemName
            Case "GASING [BLUE LAGOON]"
                Gasing(1)
            Case "NULL"
                Null(1)
            Case "BLUE SCARF"
                BlueScarf(1)
            Case "CHOCOLATE MARIO MUSHROOM"
                ChocolateMarioMashroom(1)
            Case "GOLDEN APPLE"
                GoldenApple(1)
            Case "CHAIN MAIL"
                CainMail(1)
            Case "LAMB SWEATER"
                LambSweater(1)
            Case "GREEN SHORTS"
                GreenShorts(1)
            Case "WOODEN SWORD"
                WoodenSword(1)
            Case "BINOCULARS"
                Binoculars(1)
            Case "IRON SWORD"
                IronSword(1)
            Case "DRUID'S STAFF"
                DruidStaff(1)
            Case "BOOK OF THE DAMNED"
                BookOfTheDamned(1)
        End Select

    End Sub
    Sub Inventor()
        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.001
        End If


        Dim statsMenu() As String =
            {
                "╬═══════════════════════════════════════════════════════╬",
                "║╬═════════════════════════════════════════════════════╬║",
                "║║                                                     ║║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "╬═══════════════════════════════════════════════════════╬",
                "║ INFO                                                  ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "╬═══════════════════════════════════════════════════════╬",
                "║ INVENTORY                                             ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "║                                                       ║",
                "╬═══════════════════════════════════════════════════════╬"
            }

        Dim pipInfoList() As String =
            {
                "NAME",
                "AGE",
                "GENDER",
                "INTELLECT"
            }

        Dim pipInformation() As String =
            {
                "PIP",
                "23 CENTURY",
                "SEMI-DRUID",
                "73"
            }

        Dim str, userAns As String

        Dim l As Integer

        Dim symb As Char

        Dim pass, res As Boolean

        pass = False

        Do
            Console.SetCursorPosition(121, 23)
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.WriteLine("INVENTORY")
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.DarkYellow
            DisplayingPictureFromSide(statsMenu, 57, 2, speed)
            Console.ResetColor()

            str = "mrpip"
            Console.ForegroundColor = ConsoleColor.Magenta
            If kingdom_of_snails Then
                DisplayingPictureFromSide(IssuingAnIcon(str), 61, 5, 0)
            Else
                DisplayingPictureFromSide(IssuingAnIcon(str), 61, 5, 0.0002)
            End If

            Console.ResetColor()

            'Вывод информации по поводу персонажа
            l = 5
            For i = 0 To UBound(pipInfoList)
                Console.SetCursorPosition(78, l)

                Console.ForegroundColor = ConsoleColor.Cyan
                Console.Write("| ")
                Console.ResetColor()

                str = pipInfoList(i)
                For j = 0 To pipInfoList(i).Length - 1
                    symb = str(j)
                    Console.Write(symb)
                    If Not kingdom_of_snails Then
                        Pause(0.05)
                    End If
                Next

                Console.Write(": ")

                Console.ForegroundColor = ConsoleColor.Cyan
                str = pipInformation(i)
                For j = 0 To pipInformation(i).Length - 1
                    symb = str(j)
                    Console.Write(symb)
                    If Not kingdom_of_snails Then
                        Pause(0.05)
                    End If
                Next
                Console.ResetColor()

                l += 2

                If Not kingdom_of_snails Then
                    Pause(0.05)
                End If
            Next

            result = False
            For i = 0 To UBound(playerInventory)
                If Not playerInventory(i) Then
                    result = True
                Else
                    result = False
                    Exit For
                End If
            Next

            If result Then
                Console.SetCursorPosition(69, 16)
                Console.ForegroundColor = ConsoleColor.DarkYellow
                Console.WriteLine("[ INVENTORY EMPTY | BUT VERY CLEAN! ]")
                Console.ResetColor()
            Else
                Console.SetCursorPosition(69, 16)
                Console.ForegroundColor = ConsoleColor.DarkYellow
                Console.WriteLine("[ ENTER ITEM NUMBER TO VIEW INFO ]")
                Console.SetCursorPosition(69, 17)
                Console.WriteLine("[ ENTER A CAPITAL LETTER - TO SELL IT ]")
                Console.SetCursorPosition(69, 19)
                Console.WriteLine("[ ENTER A SMALL LETTER - USE IT ]")
                Console.ResetColor()
                l = 23
                Console.ForegroundColor = ConsoleColor.Cyan
                For i = 0 To UBound(playerInventory)
                    Console.SetCursorPosition(61, l)
                    Console.Write("{0}| ", i + 1)

                    If playerInventory(i) Then
                        Console.Write(playerInventoryNames(i))
                        Select Case playerInventoryNames(i)
                            Case "BLUE SCARF"
                                Console.Write(" [A]")
                            Case "CHOCOLATE MARIO MUSHROOM"
                                Console.Write(" [B/b]")
                            Case "GOLDEN APPLE"
                                Console.Write(" [C]")
                            Case "CHAIN MAIL"
                                Console.Write(" [D]")
                            Case "LAMB SWEATER"
                                Console.Write(" [E]")
                            Case "GREEN SHORTS"
                                Console.Write(" [F]")
                            Case "GASING [BLUE LAGOON]"
                                Console.Write(" [G/g]")
                            Case "WOODEN SWORD"
                                Console.Write(" [H]")
                            Case "BINOCULARS"
                                Console.Write(" [I]")
                            Case "IRON SWORD"
                                Console.Write(" [J]")
                            Case "DRUID'S STAFF"
                                Console.Write(" [K]")
                            Case "BOOK OF THE DAMNED"
                                Console.Write(" [L]")
                        End Select
                    Else
                        playerInventoryNames(i) = "NULL"
                        Console.Write("NULL")
                    End If

                    l += 2
                Next
                Console.ResetColor()
            End If

            Console.SetCursorPosition(78, 13)
            Console.ForegroundColor = ConsoleColor.Yellow
            Console.WriteLine("[ ENTER ""0"" (ZERO) TO RETURN ]")
            Console.ResetColor()

            Console.SetCursorPosition(59, 18)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            If result Then
                res = CheckUserAns(userAns, 0, 0)
                If Not res Then
                    speed = 0
                    pass = False
                Else
                    pass = True
                    If userAns = 0 Then
                        Tavern()
                    End If
                End If
            Else
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventoryNames(i) = "NULL"
                    End If
                Next
                Select Case userAns
                    Case 0
                        Tavern()
                    Case 1
                        PlayerItemsInfo(playerInventoryNames(0))
                    Case 2
                        PlayerItemsInfo(playerInventoryNames(1))
                    Case 3
                        PlayerItemsInfo(playerInventoryNames(2))
                    Case 4
                        PlayerItemsInfo(playerInventoryNames(3))
                    Case 5
                        PlayerItemsInfo(playerInventoryNames(4))
                    Case 6
                        PlayerItemsInfo(playerInventoryNames(5))
                    Case "A"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "BLUE SCARF" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 300
                                playerBag -= 1
                                maxPlayerXp -= 20

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "B"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "CHOCOLATE MARIO MUSHROOM" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 450
                                playerBag -= 1

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "C"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "GOLDEN APPLE" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 110
                                playerBag -= 1

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "D"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "CHAIN MAIL" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 60
                                playerBag -= 1
                                maxPlayerXp -= 25

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "E"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "LAMB SWEATER" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 40
                                playerBag -= 1
                                maxPlayerXp -= 15

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "F"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "GREEN SHORTS" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 30
                                playerBag -= 1
                                maxPlayerXp -= 5

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "G"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "GASING [BLUE LAGOON]" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 20
                                playerBag -= 1

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "H"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "WOODEN SWORD" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 50
                                playerBag -= 1
                                maxPlayerXp -= 5

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "I"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "BINOCULARS" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 40
                                playerBag -= 1
                                maxPlayerXp -= 10

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "J"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "IRON SWORD" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 120
                                playerBag -= 1
                                maxPlayerXp -= 10

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "K"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "DRUID'S STAFF" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 1400
                                playerBag -= 1
                                maxPlayerXp -= 50

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "L"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "BOOK OF THE DAMNED" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerCoins += 4750
                                playerBag -= 1
                                maxPlayerXp -= 75

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                    Case "b"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "CHOCOLATE MARIO MUSHROOM" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerBag -= 1
                                maxPlayerXp += 30

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                        playerBag -= 1
                    Case "g"
                        For i = 0 To UBound(playerInventory)
                            If playerInventoryNames(i) = "GASING [BLUE LAGOON]" Then
                                playerInventory(i) = False
                                playerInventoryNames(i) = ""
                                playerBag -= 1
                                If playerHp + 25 > maxPlayerXp Then
                                    playerHp = maxPlayerXp
                                Else
                                    playerHp += 25
                                End If
                                playerSoda = maxPlayerSoda

                                If playerCoins >= 2000 Then
                                    done_first_season_tasks(4) = True
                                End If
                                Tavern()
                            End If
                        Next
                        playerBag -= 1
                End Select
            End If
        Loop Until pass

    End Sub
    Sub IronChest()
        Console.WindowHeight = 40
        Console.WindowWidth = 141
        Console.SetBufferSize(141, 40)
        Console.Clear()

        If Not kingdom_of_snails Then
            speed = 0.0035
        Else
            speed = 0
        End If


        Dim ironPic() As String =
            {
                "╔══╗╔═══╗╔══╗╔╗─╔╗ ╔══╗╔╗╔╗╔═══╗╔══╗╔════╗",
                "╚╗╔╝║╔═╗║║╔╗║║╚═╝║ ║╔═╝║║║║║╔══╝║╔═╝╚═╗╔═╝",
                "─║║─║╚═╝║║║║║║╔╗─║ ║║──║╚╝║║╚══╗║╚═╗──║║  ",
                "─║║─║╔╗╔╝║║║║║║╚╗║ ║║──║╔╗║║╔══╝╚═╗║──║║  ",
                "╔╝╚╗║║║║─║╚╝║║║─║║ ║╚═╗║║║║║╚══╗╔═╝║──║║  ",
                "╚══╝╚╝╚╝─╚══╝╚╝─╚╝ ╚══╝╚╝╚╝╚═══╝╚══╝──╚╝  "
            }

        Dim chestPicClose() As String =
            {
                "█▀▀▀▀█▄▄▄▄▄▄▄▄▄▄▄█▀▀▀▀█▄▄▄▄▄▄▄▄▄▄▄█▀▀▀▀█",
                "█░░░░█████████████░░░░█████████████░░░░█",
                "█▄▄▄▄█████████████▄▄▄▄█████████████▄▄▄▄█",
                "█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█",
                "█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█",
                "█▀▀▀▀▀▀▀▀▀▀▀▀▀▀█▀▀▀▀▀▀▀▀█▀▀▀▀▀▀▀▀▀▀▀▀▀▀█",
                "█░░░░░░░░░░░░░░█░░░██░░░█░░░░░░░░░░░░░░█",
                "█▄▄▄▄▄▄▄▄▄▄▄▄▄▄█░░░▀▀░░░█▄▄▄▄▄▄▄▄▄▄▄▄▄▄█",
                "█░░░░████████████▄▄▄▄▄▄████████████░░░░█",
                "█░░░░█████████████▀▀▀▀█████████████░░░░█",
                "█░░░░█████████████░░░░█████████████░░░░█",
                "█▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█",
                "█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█",
                "█▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█"
            }

        Dim openChestPic() As String =
            {
                "█▀▀▀▀█▄▄▄▄▄▄▄▄▄▄▄█▀▀▀▀█▄▄▄▄▄▄▄▄▄▄▄█▀▀▀▀█",
                "█░░░░█████████████░░░░█████████████░░░░█",
                "█▄▄▄▄█████████████▄▄▄▄█████████████▄▄▄▄█",
                "█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█",
                "█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█",
                "█▀▀▀▀▀▀▀▀▀▀▀▀▀▀█▀▀▀▀▀▀▀▀█▀▀▀▀▀▀▀▀▀▀▀▀▀▀█",
                "█░░░░░░░░░░░░░░█░░░██░░░█░░░░░░░░░░░░░░█",
                "█▄▄▄▄▄▄▄▄▄▄▄▄▄▄█░░░▀▀░░░█▄▄▄▄▄▄▄▄▄▄▄▄▄▄█",
                "                                        ",
                "                                        ",
                "                                        ",
                "                                        ",
                "                                        ",
                "                                        ",
                "█░░░░████████████▄▄▄▄▄▄████████████░░░░█",
                "█░░░░█████████████▀▀▀▀█████████████░░░░█",
                "█░░░░█████████████░░░░█████████████░░░░█",
                "█▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█",
                "█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█",
                "█▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█"
            }

        Dim infoItems() As String =
            {
                "[ CHOOSE 1 OF THREE POSSIBLE CHESTS TO OPEN ]",
                "[ #1 CHEST - ATTACK SET ]",
                "[ #2 CHEST - PROTECTION SET ]",
                "[ #3 CHEST (BIG CHEST) - A SET OF THINGS FOR ATTACK AND PROTECTION]",
                "[ ENTER ITEM LETTER TO SEE INFORMATION ABOUT IT ]",
                "[ ENTER ""0"" (ZERO) TO RETURN ]"
            }

        Dim items() As String =
            {
                "═══════════════════════════════",
                "| ITEMS TO PROTECT            |",
                "═══════════════════════════════",
                "                               ",
                " - BLUE SCARF [A]              ", ' #1
                " - CHOCOLATE MARIO MUSHROOM [B]", ' #2
                " - GOLDEN APPLE [C]            ", ' #3
                " - CHAIN MAIL [D]              ", ' #4
                " - LAMB SWEATER [E]            ", ' #5
                " - GREEN SHORTS [F]            ", ' #6
                " - BLUE LAGOON SODA [G]        ", ' #7
                "                               ",
                "═══════════════════════════════",
                "| ITEMS TO ATTACK             |",
                "═══════════════════════════════",
                "                               ",
                " - WOODEN SWORD [H]            ", ' #8
                " - BINOCULARS [I]              ", ' #9
                " - IRON SWORD [J]              ", ' #10
                " - DRUID'S STAFF [K]           ", ' #11
                " - BOOK OF THE DAMNED [L]      ", ' #12
                " - BLUE LAGOON SODA [M]        ", ' #7
                "                               ",
                "═══════════════════════════════"
            }

        Dim chestCost() As String =
            {
                "═════════════════════════════",
                "| CHEST PRICES              |",
                "═════════════════════════════",
                "                             ",
                "                             ",
                " - CHEST TO ATTACK: 300 C    ",
                "                             ",
                " - CHEST TO PROTECT: 150 C   ",
                "                             ",
                " - BIG CHEST: 125 C          ",
                "                             ",
                "                             ",
                "═════════════════════════════",
                "| CHEST NUMBERS             |",
                "═════════════════════════════",
                "                             ",
                "                             ",
                " | 1 | CHEST TO ATTACK       ",
                "                             ",
                " | 2 | CHEST TO PROTECT      ",
                "                             ",
                " | 3 | BIG CHEST             ",
                "                             ",
                "═════════════════════════════"
            }

        Dim mForRndChest2() As Integer = {4, 5, 1, 7, 6, 6, 5, 6, 4, 4, 3, 7, 1, 4, 7, 7, 2, 6, 3, 7, 4, 6}
        Dim mForRndChest1() As Integer = {7, 8, 9, 10, 8, 8, 9, 8, 7, 10, 7, 9, 12, 7, 8, 7, 11, 9, 10, 7, 9, 10}
        Dim mForRndChest3() As Integer = {7, 8, 7, 5, 9, 7, 9, 1, 7, 6, 7, 11, 7, 2, 5, 12, 7, 6, 4, 7, 3, 7, 6, 7, 7, 1, 10, 7, 6, 4, 7, 12, 2, 6, 3, 7, 8, 7, 11, 7, 9}
        Dim l, rndNum, num As Integer
        Dim str, userAns As String
        Dim symb As Char
        Dim pass As Boolean

        Check_tasks_for_complete()

        Do
            'Вывод |Iron chest|
            Console.SetCursorPosition(0, 3)
            Console.ForegroundColor = ConsoleColor.Gray
            ImageOutputLineByLine(ironPic, speed + 0.01, 95)
            Console.ResetColor()

            'Вывод изображения сундука
            Console.ForegroundColor = ConsoleColor.Gray
            DisplayingPictureFromSide(chestPicClose, 96, 24, speed - 0.003)
            Console.ResetColor()

            'Вывод информационного раздела
            l = 3
            For i = 0 To UBound(infoItems)
                str = infoItems(i)
                If i = UBound(infoItems) Then
                    LineOutPutCharacterByCharacter(str, 3, l + 1, speed + 0.005, ConsoleColor.Yellow)
                Else
                    LineOutPutCharacterByCharacter(str, 3, l, speed + 0.005, ConsoleColor.Yellow)
                End If
                l += 1
            Next

            'Вывод информации про игрока
            Console.SetCursorPosition(95, 15)
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write("[ PLAYER'S COINS: ")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.Write("{0}", playerCoins)
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write(" ]")
            Console.ResetColor()
            Console.SetCursorPosition(95, 17)
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write("[ BAG: ")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.Write("{0}", playerBag)
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write(" / ")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.Write("{0}", maxPlayerBag)
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write(" ]")
            Console.ResetColor()

            'Вывод возможных предметов
            Console.ForegroundColor = ConsoleColor.DarkCyan
            DisplayingPictureFromSide(items, 3, 14, speed - 0.002)
            Console.ResetColor()
            'Вывод информации про сундуки
            Console.ForegroundColor = ConsoleColor.Magenta
            DisplayingPictureFromSide(chestCost, 38, 14, speed - 0.002)
            Console.ResetColor()
            'Вывод инвентаря игрока
            Console.SetCursorPosition(69, 25)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.Write("[ INBENTORY ]")
            l = 27
            For i = 0 To UBound(playerInventory)
                Console.SetCursorPosition(69, l)
                Console.Write("{0}| ", i + 1)

                If playerInventory(i) Then
                    Console.Write(playerInventoryNames(i))
                Else
                    Console.Write("NULL")
                End If

                l += 2
            Next
            Console.ResetColor()

            Console.SetCursorPosition(95, 20)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            Select Case userAns
                Case "A"
                    BlueScarf(2)
                Case "B"
                    ChocolateMarioMashroom(2)
                Case "C"
                    GoldenApple(2)
                Case "D"
                    CainMail(2)
                Case "E"
                    LambSweater(2)
                Case "F"
                    GreenShorts(2)
                Case "G"
                    Gasing(2)
                Case "H"
                    WoodenSword(2)
                Case "I"
                    Binoculars(2)
                Case "J"
                    IronSword(2)
                Case "K"
                    DruidStaff(2)
                Case "L"
                    BookOfTheDamned(2)
                Case "M"
                    Gasing(2)
                Case 1
                    If playerCoins >= 300 And playerBag < 6 Then
                        pass = True
                        playerCoins -= 300
                        playerBag += 1
                        str = "[ PURCHASE DONE SUCCESSFULLY ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Green)
                        Pause(2)
                        Console.Clear()
                    ElseIf playerCoins >= 300 And playerBag >= 6 Then
                        pass = False
                        speed = 0
                        str = "[ INSUFFICIENT INVENTORY SPACE ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    ElseIf playerBag < 6 And playerCoins < 300 Then
                        pass = False
                        speed = 0
                        str = "[ NOT ENOUGH COINS TO BUY ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    Else
                        pass = False
                        speed = 0
                        str = "[ NOT ENOUGH COINS TO BUY | INSUFFICIENT INVENTORY SPACE ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case 2
                    If playerCoins >= 150 And playerBag < 6 Then
                        pass = True
                        playerCoins -= 150
                        playerBag += 1
                        str = "[ PURCHASE DONE SUCCESSFULLY ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Green)
                        Pause(2)
                        Console.Clear()
                    ElseIf playerCoins >= 150 And playerBag >= 6 Then
                        pass = False
                        speed = 0
                        str = "[ INSUFFICIENT INVENTORY SPACE ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    ElseIf playerBag < 6 And playerCoins < 150 Then
                        pass = False
                        speed = 0
                        str = "[ Not ENOUGH COINS To BUY ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    Else
                        pass = False
                        speed = 0
                        str = "[ NOT ENOUGH COINS TO BUY | INSUFFICIENT INVENTORY SPACE ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case 3
                    If playerCoins >= 125 And playerBag < 6 Then
                        pass = True
                        playerCoins -= 125
                        playerBag += 1
                        Console.SetCursorPosition(3, 11)
                        Console.ForegroundColor = ConsoleColor.Green
                        str = "[ PURCHASE DONE SUCCESSFULLY ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Green)
                        Console.ResetColor()
                        Pause(2)
                        Console.Clear()
                    ElseIf playerCoins >= 125 And playerBag >= 6 Then
                        pass = False
                        speed = 0
                        str = "[ INSUFFICIENT INVENTORY SPACE ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    ElseIf playerBag < 6 And playerCoins < 125 Then
                        pass = False
                        speed = 0
                        str = "[ NOT ENOUGH COINS To BUY ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    Else
                        pass = False
                        speed = 0
                        str = "[ NOT ENOUGH COINS TO BUY | INSUFFICIENT INVENTORY SPACE ]"
                        LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                        Pause(2)
                        Console.Clear()
                    End If
                Case 0
                    Tavern()
                Case Else
                    speed = 0
                    str = "[ INCORRECT VALUE ]"
                    LineOutPutCharacterByCharacter(str, 95, 20, 0.004, ConsoleColor.Red)
                    Pause(2)
                    Console.Clear()
            End Select

        Loop Until pass

        'Открытие сундука
        Console.Clear()

        'Вывод сундука по центру
        Console.WriteLine(Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10))
        Console.ForegroundColor = ConsoleColor.Gray
        ImageOutputLineByLine(chestPicClose, speed, 50)
        Console.ResetColor()

        'Рандом элемента массива
        Randomize()
        Select Case userAns
            Case 1
                rndNum = Rnd() * (UBound(mForRndChest1) - 0) + 0
                num = mForRndChest1(rndNum)
            Case 2
                rndNum = Rnd() * (UBound(mForRndChest2) - 0) + 0
                num = mForRndChest2(rndNum)
            Case 3
                rndNum = Rnd() * (UBound(mForRndChest3) - 0) + 0
                num = mForRndChest3(rndNum)
        End Select

        Select Case num
            Case 1
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "BLUE SCARF"
                        maxPlayerXp += 20
                        num = i
                        Exit For
                    End If
                Next
            Case 2
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "CHOCOLATE MARIO MUSHROOM"
                        num = i
                        Exit For
                    End If
                Next
            Case 3
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "GOLDEN APPLE"
                        num = i
                        Exit For
                    End If
                Next
            Case 4
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "CHAIN MAIL"

                        maxPlayerXp += 25
                        num = i
                        Exit For
                    End If
                Next
            Case 5
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "LAMB SWEATER"
                        maxPlayerXp += 15
                        num = i
                        Exit For
                    End If
                Next
            Case 6
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "GREEN SHORTS"
                        maxPlayerXp += 5
                        num = i
                        Exit For
                    End If
                Next
            Case 7
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "GASING [BLUE LAGOON]"
                        num = i
                        Exit For
                    End If
                Next
            Case 8
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "WOODEN SWORD"
                        maxPlayerXp += 5
                        num = i
                        Exit For
                    End If
                Next
            Case 9
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "BINOCULARS"
                        maxPlayerXp += 10
                        num = i
                        Exit For
                    End If
                Next
            Case 10
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "IRON SWORD"
                        maxPlayerXp += 10
                        num = i
                        Exit For
                    End If
                Next
            Case 11
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "DRUID'S STAFF"
                        maxPlayerXp += 50
                        num = i
                        Exit For
                    End If
                Next
            Case 12
                For i = 0 To UBound(playerInventory)
                    If Not playerInventory(i) Then
                        playerInventory(i) = True
                        playerInventoryNames(i) = "BOOK OF THE DAMNED"
                        maxPlayerXp += 75
                        num = i
                        Exit For
                    End If
                Next
        End Select

        Pause(3)
        Console.Clear()
        'Выводим открытый сундук
        Console.WriteLine(Chr(10) & Chr(10) & Chr(10) & Chr(10) & Chr(10))
        Console.ForegroundColor = ConsoleColor.Gray
        ImageOutputLineByLine(openChestPic, speed, 50)
        Console.ResetColor()

        '40, 30
        str = "simpleDialogWindow"
        'Вывод диалогового окна
        DisplayingPictureFromSide(DisplayingDialogBox(str), 40, 30, speed)

        str = "iconlars"
        'Вывод иконки персонажа
        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(IssuingAnIcon(str), 26, 30, speed)
        Console.ResetColor()

        'Вывод имени персонажа
        Console.SetCursorPosition(43, 31)
        str = "LARS"
        Console.ForegroundColor = ConsoleColor.DarkYellow
        For i = 0 To str.Length - 1
            symb = str(i)
            Console.Write(symb)
            Pause(0.05)
        Next
        Console.ResetColor()

        'Вывод реплики
        Console.SetCursorPosition(43, 33)
        str = "MY CONGRATS! YOU RECEIVED: "
        Console.ForegroundColor = ConsoleColor.Yellow
        For i = 0 To str.Length - 1
            symb = str(i)
            Console.Write(symb)
            Pause(0.05)
        Next
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write("{0}", playerInventoryNames(num))
        Console.ResetColor()

        Console.SetCursorPosition(43, 35)
        Console.ForegroundColor = ConsoleColor.Yellow
        Console.Write("[ PRESS ""ENTER"" TO RETURN ]")
        Console.ResetColor()

        Console.ReadKey()

        If playerCoins >= 2000 Then
            done_first_season_tasks(4) = True
        End If

        IronChest()
    End Sub
    Sub First_season_tasks_fird_page(pic_letter_t() As String, pic_letter_a() As String, pic_letter_s() As String, pic_letter_k() As String, task_card() As String, first_season_tasks_names() As String, first_season_tasks_info() As String, info_table() As String)
        Console.WindowHeight = 40
        Console.WindowWidth = 141
        Console.SetBufferSize(141, 40)
        Console.Clear()

        speed = 0.0045

        Dim pic_fird_list() As String =
           {
               "╔══╦══╦═══╦══╗─ ╔╗─╔══╦══╦════╗",
               "║╔═╩╗╔╣╔═╗║╔╗╚╗ ║║─╚╗╔╣╔═╩═╗╔═╝",
               "║╚═╗║║║╚═╝║║╚╗║ ║║──║║║╚═╗─║║  ",
               "║╔═╝║║║╔╗╔╣║─║║ ║║──║║╚═╗║─║║  ",
               "║║─╔╝╚╣║║║║╚═╝║ ║╚═╦╝╚╦═╝║─║║  ",
               "╚╝─╚══╩╝╚╝╚═══╝ ╚══╩══╩══╝─╚╝  "
           }

        Dim info_table_for_fird_page_first_season() As String =
            {
                "[ ENTER 0 ""ZERO"" TO RETURN TO THE TAVERN ]",
                "[ ENTER 1 ""ONE"" TO GO TO THE SECOND LIST ]"
            }

        Dim reward_tasks_fird_list() = {1100, 3500, 3500, 7000}

        Dim x, y, special_counter As Integer
        Dim user_answer, str As String

        Do
            Console.ForegroundColor = ConsoleColor.White
            Console.SetCursorPosition(0, 2)
            ImageOutputLineByLine(pic_letter_t, speed + 0.012, 128)
            Console.SetCursorPosition(0, 9)
            ImageOutputLineByLine(pic_letter_a, speed + 0.012, 128)
            Console.SetCursorPosition(0, 16)
            ImageOutputLineByLine(pic_letter_s, speed + 0.012, 128)
            Console.SetCursorPosition(0, 23)
            ImageOutputLineByLine(pic_letter_k, speed + 0.012, 128)
            Console.SetCursorPosition(0, 30)
            ImageOutputLineByLine(pic_letter_s, speed + 0.012, 128)
            Console.ResetColor()

            Pause(1)

            x = 96
            Console.ForegroundColor = ConsoleColor.Gray
            For i = 0 To 3
                Console.SetCursorPosition(0, 11)
                ImageOutputLineByLine(task_card, speed + 0.01, x)
                x -= 30
            Next
            Console.ResetColor()

            Console.SetCursorPosition(0, 3)
            Console.ForegroundColor = ConsoleColor.DarkRed
            ImageOutputLineByLine(pic_fird_list, speed + 0.03, 10)
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.Yellow
            Console.SetCursorPosition(6, 38)
            Console.Write(info_table(0))
            Console.SetCursorPosition(6, 36)
            Console.Write(info_table(1))
            Console.ResetColor()

            'Первая ячейка
            Console.SetCursorPosition(9, 32)
            If done_first_season_tasks(8) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 1100 C |")
            'Состояние задания
            Console.SetCursorPosition(9, 11)
            If playerCoins >= 2000 Then
                done_first_season_tasks(8) = True
            End If
            If Not done_first_season_tasks(8) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(9, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(9, y)
            For i = 0 To first_season_tasks_names(8).Length - 1
                Console.SetCursorPosition(9 + special_counter, y)
                Console.Write(first_season_tasks_names(8)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Информация о задании | special_counter
            Console.SetCursorPosition(9, 22)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 24
            Console.SetCursorPosition(9, y)
            For i = 0 To first_season_tasks_info(8).Length - 1
                Console.SetCursorPosition(9 + special_counter, y)
                Console.Write(first_season_tasks_info(8)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Игровой прогресс для задания
            Console.SetCursorPosition(9, 28)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            Console.SetCursorPosition(9, 30)
            Console.ForegroundColor = ConsoleColor.Cyan
            If done_first_season_tasks(8) Then
                Console.WriteLine("| 1 / 1 |")
            Else
                Console.WriteLine("| 0 / 1 |")
            End If
            Console.ResetColor()
            'Вторая ячейка
            Console.SetCursorPosition(39, 32)
            If done_first_season_tasks(9) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 3500 C |")
            'Состояние задания
            'Игровой прогресс для задания
            Console.SetCursorPosition(39, 24)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            Dim amount_of_card_games_from_5 As Byte
            For i = 0 To UBound(card_and_standart_games, 2)
                If card_and_standart_games(0, i) Then
                    amount_of_card_games_from_5 += 1
                Else
                    Exit For
                End If
            Next
            Console.SetCursorPosition(39, 26)
            Console.ForegroundColor = ConsoleColor.Cyan
            If amount_of_card_games_from_5 = 5 Or done_first_season_tasks(9) Then
                Console.WriteLine("| 5 / 5 |")
            Else
                Console.WriteLine("| {0} / 5 |", amount_of_card_games_from_5)
            End If
            Console.ResetColor()
            'Информация о задании | special_counter
            Console.SetCursorPosition(39, 18)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 20
            Console.SetCursorPosition(39, y)
            For i = 0 To first_season_tasks_info(9).Length - 1
                Console.SetCursorPosition(39 + special_counter, y)
                Console.Write(first_season_tasks_info(9)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            Console.SetCursorPosition(39, 11)
            If Not done_first_season_tasks(9) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(39, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(39, y)
            For i = 0 To first_season_tasks_names(9).Length - 1
                Console.SetCursorPosition(39 + special_counter, y)
                Console.Write(first_season_tasks_names(9)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next

            'Третья ячейка
            Console.SetCursorPosition(69, 32)
            If done_first_season_tasks(10) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 3500 C |")
            'Состояние задания
            'Игровой прогресс для задания
            Console.SetCursorPosition(69, 24)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            Dim amount_of_standart_games_from_5 As Byte
            For i = 0 To UBound(card_and_standart_games, 2)
                If card_and_standart_games(1, i) Then
                    amount_of_standart_games_from_5 += 1
                Else
                    Exit For
                End If
            Next
            Console.SetCursorPosition(69, 26)
            Console.ForegroundColor = ConsoleColor.Cyan
            If amount_of_standart_games_from_5 = 5 Or done_first_season_tasks(10) Then
                Console.WriteLine("| 5 / 5 |")
            Else
                Console.WriteLine("| {0} / 5 |", amount_of_standart_games_from_5)
            End If
            Console.ResetColor()

            Console.SetCursorPosition(69, 11)
            If Not done_first_season_tasks(10) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(69, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(69, y)
            For i = 0 To first_season_tasks_names(10).Length - 1
                Console.SetCursorPosition(69 + special_counter, y)
                Console.Write(first_season_tasks_names(10)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Информация о задании
            Console.SetCursorPosition(69, 20)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 22
            Console.SetCursorPosition(69, y)
            For i = 0 To first_season_tasks_info(10).Length - 1
                Console.SetCursorPosition(69 + special_counter, y)
                Console.Write(first_season_tasks_info(10)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next

            'Четвёртая ячейка
            Console.SetCursorPosition(99, 32)
            If done_first_season_tasks(11) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 7000 C |")
            'Состояние задания
            'Игровой прогресс для задания
            Console.SetCursorPosition(99, 26)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            Dim good_puppy As Boolean
            Dim amount_of_done_chapters As Byte
            good_puppy = True
            For i = 0 To UBound(doneChapters)
                If Not doneChapters(i) Then
                    good_puppy = False
                Else
                    amount_of_done_chapters += 1
                End If
            Next
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.SetCursorPosition(99, 28)
            If good_puppy Then
                done_first_season_tasks(11) = True
                Console.WriteLine("| 3 / 3 |")
            Else
                Console.WriteLine("| {0} / 3 |", amount_of_done_chapters)
            End If
            Console.ResetColor()
            Console.SetCursorPosition(99, 11)
            If Not done_first_season_tasks(11) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(99, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(99, y)
            For i = 0 To first_season_tasks_names(11).Length - 1
                Console.SetCursorPosition(99 + special_counter, y)
                Console.Write(first_season_tasks_names(11)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Информация о задании | special_counter
            Console.SetCursorPosition(99, 20)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 22
            Console.SetCursorPosition(99, y)
            For i = 0 To first_season_tasks_info(11).Length - 1
                Console.SetCursorPosition(99 + special_counter, y)
                Console.Write(first_season_tasks_info(11)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next

            Console.SetCursorPosition(66, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_answer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_answer, 0, 1)

            If result Then
                speed = 0.0045
                Select Case user_answer
                    Case "0"
                        Tavern()
                    Case "1"
                        'Переход ко второму списку
                        first_season_tasks_second_page(pic_letter_t, pic_letter_a, pic_letter_s, pic_letter_k, task_card, first_season_tasks_names, first_season_tasks_info, info_table)
                End Select
            Else
                speed = 0.0045
                str = "[ INVALID VALUE ]"
                LineOutPutCharacterByCharacter(str, 66, 38, speed + 0.01, ConsoleColor.Red)
                Pause(2)
                Console.Clear()
            End If
        Loop

    End Sub
    Sub First_season_tasks_second_page(pic_letter_t() As String, pic_letter_a() As String, pic_letter_s() As String, pic_letter_k() As String, task_card() As String, first_season_tasks_names() As String, first_season_tasks_info() As String, info_table() As String)
        Console.WindowHeight = 40
        Console.WindowWidth = 141
        Console.SetBufferSize(141, 40)
        Console.Clear()

        speed = 0.0045

        Dim pic_second_list() As String =
            {
                "╔══╦═══╦══╦══╦╗─╔╦══╗  ╔╗─╔══╦══╦════╗",
                "║╔═╣╔══╣╔═╣╔╗║╚═╝║╔╗╚╗ ║║─╚╗╔╣╔═╩═╗╔═╝",
                "║╚═╣╚══╣║─║║║║╔╗─║║╚╗║ ║║──║║║╚═╗─║║",
                "╚═╗║╔══╣║─║║║║║╚╗║║─║║ ║║──║║╚═╗║─║║",
                "╔═╝║╚══╣╚═╣╚╝║║─║║╚═╝║ ║╚═╦╝╚╦═╝║─║║",
                "╚══╩═══╩══╩══╩╝─╚╩═══╝ ╚══╩══╩══╝─╚╝"
            }
        Dim reward_tasks_second_list() = {1000, 750, 750, 750}

        Dim x, y, special_counter As Integer
        Dim user_answer, str As String
        Dim counter As Byte

        Do
            Console.ForegroundColor = ConsoleColor.White
            Console.SetCursorPosition(0, 2)
            ImageOutputLineByLine(pic_letter_t, speed + 0.012, 128)
            Console.SetCursorPosition(0, 9)
            ImageOutputLineByLine(pic_letter_a, speed + 0.012, 128)
            Console.SetCursorPosition(0, 16)
            ImageOutputLineByLine(pic_letter_s, speed + 0.012, 128)
            Console.SetCursorPosition(0, 23)
            ImageOutputLineByLine(pic_letter_k, speed + 0.012, 128)
            Console.SetCursorPosition(0, 30)
            ImageOutputLineByLine(pic_letter_s, speed + 0.012, 128)
            Console.ResetColor()

            Pause(1)

            x = 96
            Console.ForegroundColor = ConsoleColor.Gray
            For i = 0 To 3
                Console.SetCursorPosition(0, 11)
                ImageOutputLineByLine(task_card, speed + 0.01, x)
                x -= 30
            Next
            Console.ResetColor()

            Console.SetCursorPosition(0, 3)
            Console.ForegroundColor = ConsoleColor.DarkCyan
            ImageOutputLineByLine(pic_second_list, speed + 0.03, 10)
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.Yellow
            Console.SetCursorPosition(6, 38)
            Console.Write(info_table(0))
            Console.SetCursorPosition(6, 36)
            Console.Write(info_table(1))
            Console.SetCursorPosition(66, 36)
            Console.Write(info_table(2))
            Console.ResetColor()

            'Первая ячейка
            Console.SetCursorPosition(8, 32)
            If done_first_season_tasks(4) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 1000 C |")
            'Состояние задания
            Console.SetCursorPosition(8, 11)
            If playerCoins >= 2000 Then
                done_first_season_tasks(4) = True
            End If
            If Not done_first_season_tasks(4) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(9, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            Console.SetCursorPosition(9, 16)
            Console.WriteLine("| {0} |", first_season_tasks_names(4))
            'Информация о задании | special_counter
            Console.SetCursorPosition(9, 18)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 20
            Console.SetCursorPosition(9, y)
            For i = 0 To first_season_tasks_info(4).Length - 1
                Console.SetCursorPosition(9 + special_counter, y)
                Console.Write(first_season_tasks_info(4)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Игровой прогресс для задания
            Console.SetCursorPosition(9, 22)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            Console.SetCursorPosition(9, 24)
            Console.ForegroundColor = ConsoleColor.Cyan
            If Not done_first_season_tasks(4) Then
                Console.WriteLine("| {0} / 2000 |", playerCoins)
            Else
                Console.WriteLine("| 2000 / 2000 |")
            End If
            Console.ResetColor()

            'Вторая ячейка
            Console.SetCursorPosition(39, 32)
            If done_first_season_tasks(5) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 750 C |")
            'Состояние задания
            'Игровой прогресс для задания
            Console.SetCursorPosition(39, 22)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            If done_first_season_tasks(5) Then
                counter = 5
            Else
                counter = 0
                For i = 0 To UBound(defeat_3_crearures, 2)
                    If defeat_3_crearures(1, i) Then
                        counter += 1
                    Else
                        Exit For
                    End If
                Next
            End If
            Console.ForegroundColor = ConsoleColor.Cyan
            If counter <> 5 Then
                Console.SetCursorPosition(39, 24)
                Console.WriteLine("| {0} / 5 |", counter)
            Else
                Console.SetCursorPosition(39, 24)
                Console.WriteLine("| 5 / 5 |")
            End If
            Console.ResetColor()
            'Информация о задании | special_counter
            Console.SetCursorPosition(39, 18)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 20
            Console.SetCursorPosition(39, y)
            For i = 0 To first_season_tasks_info(5).Length - 1
                Console.SetCursorPosition(39 + special_counter, y)
                Console.Write(first_season_tasks_info(5)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            Console.SetCursorPosition(39, 11)
            If Not done_first_season_tasks(5) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(39, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(39, y)
            For i = 0 To first_season_tasks_names(5).Length - 1
                Console.SetCursorPosition(39 + special_counter, y)
                Console.Write(first_season_tasks_names(5)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Третья ячейка
            Console.SetCursorPosition(69, 32)
            If done_first_season_tasks(6) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 750 C |")
            'Состояние задания
            'Игровой прогресс для задания
            Console.SetCursorPosition(69, 24)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            If done_first_season_tasks(6) Then
                counter = 5
            Else
                counter = 0
                For i = 0 To UBound(defeat_3_crearures, 2)
                    If defeat_3_crearures(2, i) Then
                        counter += 1
                    Else
                        Exit For
                    End If
                Next
            End If
            Console.ForegroundColor = ConsoleColor.Cyan
            If counter <> 5 Then
                Console.SetCursorPosition(69, 26)
                Console.WriteLine("| {0} / 5 |", counter)
            Else
                Console.SetCursorPosition(69, 26)
                Console.WriteLine("| 5 / 5 |")
            End If
            Console.ResetColor()
            Console.SetCursorPosition(69, 11)
            If Not done_first_season_tasks(6) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(69, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(69, y)
            For i = 0 To first_season_tasks_names(6).Length - 1
                Console.SetCursorPosition(69 + special_counter, y)
                Console.Write(first_season_tasks_names(6)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Информация о задании | special_counter
            Console.SetCursorPosition(69, 20)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 22
            Console.SetCursorPosition(69, y)
            For i = 0 To first_season_tasks_info(6).Length - 1
                Console.SetCursorPosition(69 + special_counter, y)
                Console.Write(first_season_tasks_info(6)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Четвёртая ячейка
            Console.SetCursorPosition(99, 32)
            If done_first_season_tasks(7) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 750 C |")
            'Состояние задания
            'Игровой прогресс для задания
            Console.SetCursorPosition(99, 28)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            If done_first_season_tasks(7) Then
                counter = 5
            Else
                counter = 0
                For i = 0 To UBound(defeat_3_crearures, 2)
                    If defeat_3_crearures(0, i) Then
                        counter += 1
                    Else
                        Exit For
                    End If
                Next
            End If
            Console.ForegroundColor = ConsoleColor.Cyan
            If counter <> 5 Then
                Console.SetCursorPosition(99, 30)
                Console.WriteLine("| {0} / 5 |", counter)
            Else
                Console.SetCursorPosition(99, 30)
                Console.WriteLine("| 5 / 5 |")
            End If
            Console.ResetColor()
            Console.SetCursorPosition(99, 11)
            If Not done_first_season_tasks(7) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(99, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(99, y)
            For i = 0 To first_season_tasks_names(7).Length - 1
                Console.SetCursorPosition(99 + special_counter, y)
                Console.Write(first_season_tasks_names(7)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Информация о задании | special_counter
            Console.SetCursorPosition(99, 20)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 22
            Console.SetCursorPosition(99, y)
            For i = 0 To first_season_tasks_info(7).Length - 1
                Console.SetCursorPosition(99 + special_counter, y)
                Console.Write(first_season_tasks_info(7)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next

            Console.SetCursorPosition(66, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_answer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_answer, 0, 2)

            If result Then
                speed = 0.0045
                Select Case user_answer
                    Case "0"
                        Tavern()
                    Case "1"
                        'Переход в следующую группу
                        First_season_tasks_fird_page(pic_letter_t, pic_letter_a, pic_letter_s, pic_letter_k, task_card, first_season_tasks_names, first_season_tasks_info, info_table)

                    Case "2"
                        'Переход к превому списку
                        first_season_tasks_first_page()
                End Select
            Else
                speed = 0.0045
                str = "[ INVALID VALUE ]"
                LineOutPutCharacterByCharacter(str, 66, 38, speed + 0.01, ConsoleColor.Red)
                Pause(2)
                Console.Clear()
            End If
        Loop

    End Sub
    Sub First_season_tasks_first_page()
        Console.WindowHeight = 40
        Console.WindowWidth = 141
        Console.SetBufferSize(141, 40)
        Console.Clear()

        speed = 0.0045

        Dim pic_letter_t() As String =
            {
                "████████╗",
                "╚══██╔══╝",
                "   ██║   ",
                "   ██║   ",
                "   ██║   ",
                "   ╚═╝   "
            }

        Dim pic_letter_a() As String =
            {
                " █████╗ ",
                "██╔══██╗",
                "███████║",
                "██╔══██║",
                "██║  ██║",
                "╚═╝  ╚═╝"
            }

        Dim pic_letter_s() As String =
            {
                " ██████╗",
                "██╔════╝",
                "╚█████╗ ",
                " ╚═══██╗",
                "██████╔╝",
                "╚═════╝ "
            }

        Dim pic_letter_k() As String =
            {
                "██╗  ██╗",
                "██║ ██╔╝",
                "█████═╝ ",
                "██╔═██╗ ",
                "██║ ╚██╗",
                "╚═╝  ╚═╝"
            }

        Dim task_card() As String =
            {
                "╬═══════════════════════║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║│                     │║",
                "║═══════════════════════╬"
            }

        Dim pic_first_list() As String =
            {
                "╔══╗╔══╗╔═══╗╔══╗╔════╗ ╔╗ ─╔══╗╔══╗╔════╗",
                "║╔═╝╚╗╔╝║╔═╗║║╔═╝╚═╗╔═╝ ║║──╚╗╔╝║╔═╝╚═╗╔═╝",
                "║╚═╗─║║─║╚═╝║║╚═╗──║║   ║║───║║─║╚═╗──║║",
                "║╔═╝─║║─║╔╗╔╝╚═╗║──║║   ║║───║║─╚═╗║──║║",
                "║║──╔╝╚╗║║║║─╔═╝║──║║   ║╚═╗╔╝╚╗╔═╝║──║║",
                "╚╝──╚══╝╚╝╚╝─╚══╝──╚╝   ╚══╝╚══╝╚══╝──╚╝"
            }

        Dim first_season_tasks_names() As String =
            {
                "PEASANT",
                "| YOU CAN START YOUR OWN BUSINESS |",
                "| AMMUNITION SPECIALIST |",
                "| HONORABLE SIR |",
                "BARON",
                "| IT'S DISGUSTING |",
                "| I AM Not A BOX, I AM A CHEST! |",
                "| FOR LORD GARITOS AND THE PEOPLE! |",
                "| THE BOOK IS THE SALVATION OF THE SOUL |",
                "| KING OF CARDS |",
                "| WHO EVEN ADDED THIS GAME MODE? |",
                "| MAY THE DESTINED COME TRUE! |"
            }

        Dim first_season_tasks_info() As String =
            {
               "| WIN THE FIRST BATTLE |",
               "| GET 200 COINS |",
               "| FILL YOUR INVENTORY COMPLETELY |",
               "| COMPLETE THE FIRST CHAPTER |",
               "| GET 2.000 COINS |",
               "| DEFEAT 5 SLIMES |",
               "| DEFEAT 5 CHESTS |",
               "| DEFEAT 5 KNIGHTS IN THE NAME OF THE UNDEAD |",
               "| GET BOOK OF THE DAMNED |",
               "| WIN 5 CARD MATCHES |",
               "| WIN 5 MATCHES IN STANDARD MODE |",
               "| COMPLETE ALL CHAPTERS OF THE GAME |"
            }

        Dim info_table() As String =
            {
                "[ ENTER 0 ""ZERO"" TO RETURN TO THE TAVERN ]",
                "[ ENTER 1 ""ONE"" TO GO TO THE NEXT LIST ]",
                "[ ENTER 2 ""TWO"" TO GO TO THE FIRST LIST ]"
            }

        Dim reward_tasks_first_list() As Integer = {150, 200, 200, 1300}

        Dim x, y, special_counter As Integer
        Dim user_answer, str As String

        Check_tasks_for_complete()

        Do
            Console.ForegroundColor = ConsoleColor.White
            Console.SetCursorPosition(0, 2)
            ImageOutputLineByLine(pic_letter_t, speed + 0.012, 128)
            Console.SetCursorPosition(0, 9)
            ImageOutputLineByLine(pic_letter_a, speed + 0.012, 128)
            Console.SetCursorPosition(0, 16)
            ImageOutputLineByLine(pic_letter_s, speed + 0.012, 128)
            Console.SetCursorPosition(0, 23)
            ImageOutputLineByLine(pic_letter_k, speed + 0.012, 128)
            Console.SetCursorPosition(0, 30)
            ImageOutputLineByLine(pic_letter_s, speed + 0.012, 128)
            Console.ResetColor()

            Pause(1)

            x = 96
            Console.ForegroundColor = ConsoleColor.Gray
            For i = 0 To 3
                Console.SetCursorPosition(0, 11)
                ImageOutputLineByLine(task_card, speed + 0.01, x)
                x -= 30
            Next
            Console.ResetColor()

            Console.SetCursorPosition(0, 3)
            Console.ForegroundColor = ConsoleColor.DarkMagenta
            ImageOutputLineByLine(pic_first_list, speed + 0.03, 10)
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.Yellow
            Console.SetCursorPosition(6, 38)
            Console.Write(info_table(0))
            Console.SetCursorPosition(6, 36)
            Console.Write(info_table(1))
            Console.ResetColor()

            'Первая ячейка
            Console.SetCursorPosition(9, 32)
            If done_first_season_tasks(0) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 150 C |")

            'Состояние задания
            Console.SetCursorPosition(9, 11)
            If Not done_first_season_tasks(0) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(9, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            Console.SetCursorPosition(9, 16)
            Console.WriteLine("| {0} |", first_season_tasks_names(0))
            'Информация о задании | special_counter
            Console.SetCursorPosition(9, 18)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 20
            Console.SetCursorPosition(9, y)
            For i = 0 To first_season_tasks_info(0).Length - 1
                Console.SetCursorPosition(9 + special_counter, y)
                Console.Write(first_season_tasks_info(0)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Игровой прогресс для задания
            Console.SetCursorPosition(9, 24)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            Console.SetCursorPosition(9, 26)
            Console.ForegroundColor = ConsoleColor.Cyan
            If done_first_season_tasks(0) Then
                Console.WriteLine("| 1 / 1 |")
            Else
                Console.WriteLine("| 0 / 1 |")
            End If
            Console.ResetColor()
            'Вторая ячейка
            Console.SetCursorPosition(39, 32)
            If done_first_season_tasks(1) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 200 C |")
            'Состояние задания
            Console.SetCursorPosition(39, 11)
            If playerCoins >= 200 Then
                done_first_season_tasks(1) = True
            End If
            'Название задания
            Console.SetCursorPosition(39, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(39, y)
            For i = 0 To first_season_tasks_names(1).Length - 1
                Console.SetCursorPosition(39 + special_counter, y)
                Console.Write(first_season_tasks_names(1)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Информация о задании | special_counter
            Console.SetCursorPosition(39, 20)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 22
            Console.SetCursorPosition(39, y)
            For i = 0 To first_season_tasks_info(1).Length - 1
                Console.SetCursorPosition(39 + special_counter, y)
                Console.Write(first_season_tasks_info(1)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Игровой прогресс для задания
            Console.SetCursorPosition(39, 24)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            If Not done_first_season_tasks(1) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.SetCursorPosition(39, 11)
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
                Console.SetCursorPosition(39, 26)
                Console.ForegroundColor = ConsoleColor.Cyan
                Console.WriteLine("| {0} / 200 |", playerCoins)
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.SetCursorPosition(39, 11)
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
                Console.SetCursorPosition(39, 26)
                Console.ForegroundColor = ConsoleColor.Cyan
                Console.WriteLine("| 200 / 200 |", playerCoins)
                Console.ResetColor()
            End If
            'Третья ячейка
            Console.SetCursorPosition(69, 32)
            If done_first_season_tasks(2) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 200 C |")
            'Состояние задания
            Console.SetCursorPosition(69, 11)
            If Not done_first_season_tasks(2) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(69, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(69, y)
            For i = 0 To first_season_tasks_names(2).Length - 1
                Console.SetCursorPosition(69 + special_counter, y)
                Console.Write(first_season_tasks_names(2)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Информация о задании | special_counter
            Console.SetCursorPosition(69, 20)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 22
            Console.SetCursorPosition(69, y)
            For i = 0 To first_season_tasks_info(2).Length - 1
                Console.SetCursorPosition(69 + special_counter, y)
                Console.Write(first_season_tasks_info(2)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Игровой прогресс для задания
            Console.SetCursorPosition(69, 26)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            If playerBag = 6 Then
                done_first_season_tasks(2) = True
            End If

            Console.SetCursorPosition(69, 28)
            Console.ForegroundColor = ConsoleColor.Cyan
            If done_first_season_tasks(2) Then
                Console.WriteLine("| 6 / 6 |", playerBag)
            Else
                Console.WriteLine("| {0} / 6 |", playerBag)
            End If
            Console.ResetColor()
            'Четвёртая ячейка
            Console.SetCursorPosition(99, 32)
            If done_first_season_tasks(3) Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            Console.WriteLine("| REWARD: 1300 C |")
            'Состояние задания
            Console.SetCursorPosition(99, 11)
            If Not done_first_season_tasks(3) Then
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.WriteLine("| NOT COMPLETED |")
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("| COMPLETED |")
                Console.ResetColor()
            End If
            'Название задания
            Console.SetCursorPosition(99, 14)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ NAME: ]")
            Console.ResetColor()
            special_counter = 0
            y = 16
            Console.SetCursorPosition(99, y)
            For i = 0 To first_season_tasks_names(3).Length - 1
                Console.SetCursorPosition(99 + special_counter, y)
                Console.Write(first_season_tasks_names(3)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Информация о задании | special_counter
            Console.SetCursorPosition(99, 18)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ INFORMATION: ]")
            Console.ResetColor()
            special_counter = 0
            y = 20
            Console.SetCursorPosition(99, y)
            For i = 0 To first_season_tasks_info(3).Length - 1
                Console.SetCursorPosition(99 + special_counter, y)
                Console.Write(first_season_tasks_info(3)(i))

                special_counter += 1
                If special_counter > 18 Then
                    special_counter = 0
                    y += 2
                End If
            Next
            'Игровой прогресс для задания
            Console.SetCursorPosition(99, 24)
            Console.ForegroundColor = ConsoleColor.DarkYellow
            Console.WriteLine("[ PROGRESS: ]")
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.SetCursorPosition(99, 26)
            If doneChapters(0) Then
                done_first_season_tasks(3) = True
                Console.WriteLine("| 1 / 1 |")
            Else
                Console.WriteLine("| 0 / 1 |")
            End If
            Console.ResetColor()

            Console.SetCursorPosition(66, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_answer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_answer, 0, 1)

            If result Then
                speed = 0.0045
                Select Case user_answer
                    Case "0"
                        Tavern()
                    Case "1"
                        'Переход в следующую группу
                        First_season_tasks_second_page(pic_letter_t, pic_letter_a, pic_letter_s, pic_letter_k, task_card, first_season_tasks_names, first_season_tasks_info, info_table)
                End Select
            Else
                speed = 0.0045
                str = "[ INVALID VALUE ]"
                LineOutPutCharacterByCharacter(str, 66, 38, speed + 0.01, ConsoleColor.Red)
                Pause(2)
                Console.Clear()
            End If
        Loop

    End Sub
    Sub D_season()
        Console.WindowHeight = 40
        Console.WindowWidth = 141
        Console.SetBufferSize(141, 40)
        Console.Clear()

        speed = 0.0035

        Dim pic_season() As String =
            {
                "╔══╗╔═══╗╔══╗╔══╗╔══╗╔╗ ╔╗",
                "║╔═╝║╔══╝║╔╗║║╔═╝║╔╗║║╚═╝║",
                "║╚═╗║╚══╗║╚╝║║╚═╗║║║║║╔╗ ║",
                "╚═╗║║╔══╝║╔╗║╚═╗║║║║║║║╚╗║",
                "╔═╝║║╚══╗║║║║╔═╝║║╚╝║║║ ║║",
                "╚══╝╚═══╝╚╝╚╝╚══╝╚══╝╚╝ ╚╝"
            }

        Dim pic_awakening() As String =
            {
                "╔══╗╔╗╔╗╔╗╔══╗╔╗╔══╗╔═══╗╔╗─╔╗╔══╗╔╗─╔╗╔═══╗",
                "║╔╗║║║║║║║║╔╗║║║║╔═╝║╔══╝║╚═╝║╚╗╔╝║╚═╝║║╔══╝",
                "║╚╝║║║║║║║║╚╝║║╚╝║  ║╚══╗║╔╗ ║ ║║ ║╔╗ ║║║╔═╗",
                "║╔╗║║║║║║║║╔╗║║╔╗║  ║╔══╝║║╚╗║ ║║ ║║╚╗║║║╚╗║",
                "║║║║║╚╝╚╝║║║║║║║║╚═╗║╚══╗║║ ║║╔╝╚╗║║ ║║║╚═╝║",
                "╚╝╚╝╚═╝╚═╝╚╝╚╝╚╝╚══╝╚═══╝╚╝ ╚╝╚══╝╚╝ ╚╝╚═══╝"
            }

        Dim pic_of() As String =
            {
                "╔══╗╔══╗",
                "║╔╗║║╔═╝",
                "║║║║║╚═╗",
                "║║║║║╔═╝",
                "║╚╝║║║  ",
                "╚══╝╚╝  "
            }

        Dim pic_ancient() As String =
            {
                "╔══╗╔╗─╔╗╔══╗╔══╗╔═══╗╔╗ ╔╗╔════╗",
                "║╔╗║║╚═╝║║╔═╝╚╗╔╝║╔══╝║╚═╝║╚═╗╔═╝",
                "║╚╝║║╔╗ ║║║   ║║ ║╚══╗║╔╗ ║  ║║  ",
                "║╔╗║║║╚╗║║║   ║║ ║╔══╝║║╚╗║  ║║  ",
                "║║║║║║ ║║║╚═╗╔╝╚╗║╚══╗║║ ║║  ║║  ",
                "╚╝╚╝╚╝ ╚╝╚══╝╚══╝╚═══╝╚╝ ╚╝  ╚╝  "
            }

        Dim pic_evil() As String =
            {
                "╔═══╗╔╗╔╗╔══╗╔╗  ",
                "║╔══╝║║║║╚╗╔╝║║  ",
                "║╚══╗║║║║ ║║ ║║  ",
                "║╔══╝║╚╝║ ║║ ║║  ",
                "║╚══╗╚╗╔╝╔╝╚╗║╚═╗",
                "╚═══╝ ╚╝ ╚══╝╚══╝"
            }

        Dim banner() As String =
            {
                "║╬═════════════════════╬║",
                "╬═══════════════════════╬",
                "║│                     │║",
                "║│                     │║",
                "║│     ╔═══╗══════     │║",
                "║│     ╚╗╔╗║─────│     │║",
                "║│      ║║║║─╔═══╗     │║",
                "║│      ║║║║─║╔═╗║     │║",
                "║│     ╔╝╚╝║─║╚══╗     │║",
                "║│     ╚═══╝─╚══╗║     │║",
                "║│     │─────║╚═╝║     │║",
                "║│     ══════╚═══╝     │║",
                "║│                     │║",
                "║│                     │║",
                "║│  │╬════╬│           │║",
                "║│  │║    ║│ │╬════╬│  │║",
                "╬════╬    ╬═══╬    ╬════╬"
            }

        Dim str As String

        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(pic_season, 48, 22, speed)
        Console.ResetColor()

        Pause(1)

        Console.ForegroundColor = ConsoleColor.Yellow
        DisplayingPictureFromSide(pic_awakening, 48, 8, speed)
        DisplayingPictureFromSide(pic_of, 84, 15, speed)
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Magenta
        DisplayingPictureFromSide(pic_ancient, 48, 15, speed)
        DisplayingPictureFromSide(pic_evil, 75, 22, speed)
        Console.ResetColor()

        Console.ForegroundColor = ConsoleColor.DarkGray
        DisplayingPictureFromSide(banner, 10, 9, speed - 0.003)
        DisplayingPictureFromSide(banner, 105, 9, speed - 0.003)
        Console.ResetColor()

        Console.SetCursorPosition(48, 34)
        str = "[ PRESS ANY KEY TO CONTINUE ]"
        LineOutPutCharacterByCharacter(str, 54, 36, speed + 0.03, ConsoleColor.Cyan)

        Console.ReadKey()
        First_season_tasks_first_page()
    End Sub
    Sub Tavern()
        Console.WindowHeight = 40
        Console.WindowWidth = 141
        Console.SetBufferSize(141, 40)
        Console.Clear()

        If Not kingdom_of_snails Then
            speed = 0.02
        Else
            speed = 0
        End If


        Dim Lars() As String = {
            "     ╬╬╬    ",
            "    ╬╬╬╬╬   ",
            "   ╬╬╬╬╬╬╬╬ ",
            "   ║╬╬╬╬║ ╬╬",
            "  ╬     ╬ ╬╬",
            "  ║ ─ q ║  ╬",
            "  ╬  │  ║   ",
            "  ║ ── ║║│  ",
            " ││────  ││ ",
            " ││────  ││ ",
            " ││ ──   ││ ",
            " ││ ─    ││ ",
            " ││     ││  ",
            " ││    ││   ",
            " ││    ││   "
                               }
        Dim tavern() As String = {
            "█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─",
            "│███████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████",
            "██     ║                     ║        ║                ║        █│──────    █                                       ╬══════════════════════╬",
            "│█     ╬═════════════════════╬        ║                ║        █│   ─────  █                                 ██    ║ ╔╗  ╔╗╔═══╗╔╗─╔╗╔╗╔╗ ║",
            "│█     ║─────────────────────║        ║                ║        █│          █                                 ││    ║ ║║  ║║║╔══╝║╚═╝║║║║║ ║",
            "██     ║───╔══╦══╦══╗╔══╗────║        ║                ║        █│          █                  ████████      ███    ║ ║╚╗╔╝║║╚══╗║╔╗ ║║║║║ ║",
            "██     ║───║╔═╣╔╗║╔╗╚╣╔╗║────║        ╬════════════════╬        █│───       │█               ╬═══════════════════╬  ║ ║╔╗╔╗║║╔══╝║║╚╗║║║║║ ║",
            "│█     ║───║╚═╣║║║║╚╗║╚╝║────║        ║                ║       █│           │█                ╬═════╬═══════════╬   ║ ║║╚╝║║║╚══╗║║ ║║║╚╝║ ║",
            "██     ║───╚═╗║║║║║─║║╔╗║────║        ║                ║       █│   ──────  │█                ║     ║           ║   ║ ╚╝  ╚╝╚═══╝╚╝ ╚╝╚══╝ ║",
            "│█     ║───╔═╝║╚╝║╚═╝║║║║────║        ║                ║       █│           │██               ║     ║     █     ║   ╬══════════════════════╬",
            "██     ║───╚══╩══╩═══╩╝╚╝────║        ║                ║       █│ ─────   ───│█               ║ ███ ║     ███   ║   ║                      ║",
            "│█     ╬═════════════════════╬        ║                ║       █│ ───        │█               ║█████║    █████  ║   ║                      ║",
            "██                                ╬   ║                ║       █│       ─────│█               ╬═════╬═══════════╬   ║                      ║",
            "│█                               ╬║   ║                ║       █│            │█               ║     ║           ║   ║                      ║",
            "██     ┼                        ╬║║   ║                ║       █│    ────    │█               ║     ║ │█████│   ║   ║                      ║",
            "│█═════╬                ╬═╬     ║║║   ║                ║       █│       ───  │█               ╬═════╬═══════════╬   ║                      ║",
            "██████                  ║███████████  ║                ║      █│─────        │█               ║     ║     ║     ║   ║                      ║",
            "│█────                  ║══════════█  ║                ║      █│             │█               ║     ║     ║     ║   ║                      ║",
            "██     ┼                ║   ╬═╬    █  ╬                ╬      █│             │██              ╬═════╬═════╬═════╬   ║                      ║",
            "│█═════╬                ║   ║ ║║   █  ║                ║      █│  ─────       │█              ║     ║     ║     ║   ║                      ║",
            "██████                  ║   ╬═╬    █  ║                ║      █│      ─────   │█              ║     ║     ║     ║   ║                      ║",
            "│█────                  ║══════════█  ║                ║      █│              │█              ╬═════╬═══════════╬   ║                      ║",
            "██                      ║ ╬═╬  ╬═╬ █  ║                ║      █│    ─────     │█              ║     ║     │     ║   ║                      ║",
            "██████ ┼                ║ ║ ║║ ║ ║║█  ║                ║      █          ─────│█              ║     ║     │     ║   ║                      ║",
            "│█═════╬                ║ ╬═╬  ╬═╬ █  ║                ║      █│ ────         │█              ║     ║     │     ║   ║                      ║",
            "│█────                  ╬══════════█  ║                ║      █│─────         │█              ║  │  ║     │     ║   ║                      ║",
            "██                       ──────────█  ║                ║     ████████████████████             ║  │  ║     │     ║   ║                      ║",
            "│███████████████████████████████████  ║                ║    █╬══════════════════╬█            ║  ┼  ║ ─┼─ │ ─┼─ ║   ║                      ║",
            "██                            █║──║█  ║                ║   █╬                    ╬█           ║  │  ║     │     ║   ║                      ║",
            "│█                            █║──║█  ╬════════════════╬  ██║                    ║█           ║  │  ║     │     ║   ║                      ║",
            "██                            █║──║█                      ██                      ██          ║     ║     │     ║   ║                      ║",
            "│█ █████████     █████████    █║──║█                       █                      ██          ║     ║     │     ║   ║                      ║",
            "██ ═══╬█═══      ═══╬█═══     █║──║█                      ██══║═══║═══║═══║═══║═══██          ║     ║     │     ║   ║                      ║",
            "│█    ║█            ║█        █║──║█                      ██  ║   ║   ║   ║   ║   █           ╬═════╬═══════════╬   ║                      ║",
            "██    ║█            ║█        █║──║█                       █══║═══║═══║═══║═══║═══█           ║     ║           ║   ╬══════════════════════╬",
            "│█    ║█            ║█        █║──║█                      ██████████████████████████          ║█  ██║ ███ █ ██  ║   ║──────────────────────║",
            "██    ║█            ║█        █║──║█                     ████████████████████████████         ╬═════╬═══════════╬   ║──────────────────────║",
            "│███████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████",
            "█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─█─"
                                 }

        Dim str1() As String = {
            "╔╗  ╔╗╔═══╗╔╗ ╔╗╔╗╔╗",
            "║║  ║║║╔══╝║╚═╝║║║║║",
            "║╚╗╔╝║║╚══╗║╔╗ ║║║║║",
            "║╔╗╔╗║║╔══╝║║╚╗║║║║║",
            "║║╚╝║║║╚══╗║║ ║║║╚╝║",
            "╚╝  ╚╝╚═══╝╚╝ ╚╝╚══╝"
                               }

        Dim str2() As String = {
            "╔══╦══╦══╗╔══╗",
            "║╔═╣╔╗║╔╗╚╣╔╗║",
            "║╚═╣║║║║╚╗║╚╝║",
            "╚═╗║║║║║─║║╔╗║",
            "╔═╝║╚╝║╚═╝║║║║",
            "╚══╩══╩═══╩╝╚╝"
                               }

        Dim menuItems() As String =
            {
                "LARS",
                "INVENTORY",
                "DUNGEON SEASON",
                "IRON CHEST",
                "PATH MENU"
            }

        Dim l As Integer

        Dim str, userAns As String

        Dim symb As Char

        Dim pass As Boolean

        Check_tasks_for_complete()

        Do
            'Вывод внешнего вида Tavern
            Console.ForegroundColor = ConsoleColor.DarkGray
            imageOutputLineByLine(tavern, speed, 0)
            Console.ResetColor()

            'Вывод изображение Ларса
            Console.ForegroundColor = ConsoleColor.DarkCyan
            DisplayingPictureFromSide(Lars, 10, 12, speed - 0.0164)
            Console.ResetColor()

            'Вывод изображения частей огня в камине
            l = 64

            For i = 0 To 3
                Console.SetCursorPosition(l, 33)
                Console.ForegroundColor = ConsoleColor.DarkYellow
                Console.Write("█")
                Console.ResetColor()
                l += 4
                If Not kingdom_of_snails Then
                    Pause(0.2)
                End If
            Next

            l = 64

            For i = 0 To 3
                Console.SetCursorPosition(l, 31)
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.Write("█")
                Console.ResetColor()
                l += 4
                If Not kingdom_of_snails Then
                    Pause(0.2)
                End If
            Next

            'Вывод |Soda| Cyan
            Console.ForegroundColor = ConsoleColor.DarkCyan
            displayingPictureFromSide(str2, 11, 5, speed - 0.01642)
            Console.ResetColor()


            'Вывод |Menu| Золотой
            Console.ForegroundColor = ConsoleColor.DarkYellow
            displayingPictureFromSide(str1, 118, 3, speed - 0.01642)
            Console.ResetColor()

            'Вывод информации в разделе | info
            str = "- SECTION -"
            If kingdom_of_snails Then
                LineOutPutCharacterByCharacter(str, 41, 3, 0, ConsoleColor.DarkCyan)

                str = "INFORMATION"
                LineOutPutCharacterByCharacter(str, 41, 5, 0, ConsoleColor.DarkCyan)
            Else
                LineOutPutCharacterByCharacter(str, 41, 3, 0.05, ConsoleColor.DarkCyan)

                str = "INFORMATION"
                LineOutPutCharacterByCharacter(str, 41, 5, 0.05, ConsoleColor.DarkCyan)
            End If

            'Вывод info
            l = 7
            For i = 0 To 5
                Console.SetCursorPosition(39, l)

                Select Case i
                    Case 0
                        Console.ForegroundColor = ConsoleColor.DarkCyan
                        Console.Write("| XP |")
                        Console.ResetColor()
                        Console.SetCursorPosition(39, l + 1)
                        Console.ForegroundColor = ConsoleColor.DarkYellow
                        Console.Write("[ {0} / {1} ]", playerHp, maxPlayerXp)
                        Console.ResetColor()
                    Case 1
                        Console.ForegroundColor = ConsoleColor.DarkCyan
                        Console.Write("| SODA |")
                        Console.ResetColor()
                        Console.SetCursorPosition(39, l + 1)
                        Console.ForegroundColor = ConsoleColor.DarkYellow
                        Console.Write("[ {0} ]", playerSoda)
                        Console.ResetColor()
                    Case 2
                        Console.ForegroundColor = ConsoleColor.DarkCyan
                        Console.Write("| COINS |")
                        Console.ResetColor()
                        Console.SetCursorPosition(39, l + 1)
                        Console.ForegroundColor = ConsoleColor.DarkYellow
                        Console.Write("[ {0} ]", playerCoins)
                        Console.ResetColor()
                    Case 3
                        Console.ForegroundColor = ConsoleColor.DarkCyan
                        Console.Write("| STAMINA |")
                        Console.ResetColor()
                        Console.SetCursorPosition(39, l + 1)
                        Console.ForegroundColor = ConsoleColor.DarkYellow
                        Console.Write("[ {0} / 14 ]", playerStamina)
                        Console.ResetColor()
                    Case 4
                        Console.ForegroundColor = ConsoleColor.DarkCyan
                        Console.Write("| BAG |")
                        Console.ResetColor()
                        Console.SetCursorPosition(39, l + 1)
                        Console.ForegroundColor = ConsoleColor.DarkYellow
                        Console.Write("[ {0} / {1} ]", playerBag, maxPlayerBag)
                        Console.ResetColor()
                    Case 5
                        Console.ForegroundColor = ConsoleColor.DarkCyan
                        Console.Write("| IDOLS |")
                        Console.ResetColor()
                        Console.SetCursorPosition(39, l + 1)
                        Console.ForegroundColor = ConsoleColor.DarkYellow
                        Console.Write("[ {0} ]", playerIdols)
                        Console.ResetColor()
                    Case 6
                        Console.ForegroundColor = ConsoleColor.DarkCyan
                        Console.Write("| VIP STATUS |")
                        Console.ResetColor()
                        Console.SetCursorPosition(39, l + 1)
                        Console.ForegroundColor = ConsoleColor.DarkYellow
                        If Not playerVip Then
                            Console.Write("[ NOT AVAILABLE ]")
                        Else
                            Console.Write("[ ACTIVATED ]")
                        End If
                        Console.ResetColor()
                End Select

                Console.SetCursorPosition(39, l + 2)
                Console.WriteLine("════════════════")

                l += 4
            Next

            'Вывод меню
            l = 12
            For i = 0 To UBound(menuItems)
                Console.SetCursorPosition(118, l)
                If i <> 4 Then
                    Console.ForegroundColor = ConsoleColor.Cyan
                    Console.Write(i + 1 & "|" & " ")
                    Console.ResetColor()
                Else
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                    Console.Write("0" & "|" & " ")
                    Console.ResetColor()
                End If
                str = menuItems(i)

                If i <> 4 Then
                    For j = 0 To menuItems(i).Length - 1
                        symb = str(j)
                        Console.Write(symb)
                        If Not kingdom_of_snails Then
                            Pause(0.05)
                        End If

                    Next
                Else
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                    For j = 0 To menuItems(i).Length - 1
                        symb = str(j)
                        Console.Write(symb)
                        If Not kingdom_of_snails Then
                            Pause(0.05)
                        End If
                    Next
                    Console.ResetColor()
                End If

                If i = 3 Then
                    l += 3
                Else
                    l += 2
                End If

                If Not kingdom_of_snails Then
                    Pause(0.4)
                End If
            Next

            Console.SetCursorPosition(118, l)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            result = checkUserAns(userAns, 0, 4)

            If result Then
                pass = True
            Else
                speed = 0
                Console.SetCursorPosition(118, l + 2)
                str = "| INVALID VALUE |"
                Console.ForegroundColor = ConsoleColor.DarkRed
                For i = 0 To str.Length - 1
                    symb = str(i)
                    Console.Write(symb)
                    Pause(0.04)
                Next
                Console.ResetColor()

                Pause(1.1)
            End If
        Loop Until pass

        Select Case userAns
            Case 0
                'Возвращение в меню выбора пути
                pathMenu()
            Case 1
                'Переход в диалог с Ларсом
                LarsDialog()
            Case 2
                'Переход в раздел инвентаря 
                inventor()
            Case 3
                'Сезонные испытания
                d_season()
            Case 4
                'Переход в раздел сундука
                ironChest()
        End Select

    End Sub

    'Меню/Таверна|Dungeon
    Sub PathMenu()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        If Not kingdom_of_snails Then
            speed = 0.0035
        Else
            speed = 0
        End If


        Dim str1() As String = {
            "    ╬══╬══════════╬══╬          ",
            "   ╬══╬══════════╬═╬══╬         ",
            "  ╬══╬══════════╬═══╬══╬        ",
            " ╬══╬══════════╬═══╬║╬══╬       ",
            "╬═══║══════════║═══║║║╬══╬      ",
            "║═██║██████████║██═║║║║╬══╬     ",
            "║██╬════════════╬██║║║║║╬══╬    ",
            "║█▒║░╬══╬░░╬══╬░║▒█║║║║║║╬══╬   ",
            "║█▒║░║░░║░░║░░║░║▒█║║║║║║║╬══╬  ",
            "║█▒║░╬══╬░░╬══╬░║▒█║║║║║║║║╬══╬ ",
            "║██╬════════════╬███████████╬══╬",
            "║█▒║████░░░░░░░░║▒█╬══════╬█══╬ ",
            "║█▒║█░│█░░░░░░░░║▒█║░░││░░║█═╬  ",
            "║█▒║█░│█░░░░░░░░║▒█╬══════╬█    ",
            "║███████████████████████████    ",
            "  ═══──════════════════════     ",
            "   ══──════════════════         ",
            "    ═──══════════════           "
                               }

        Dim str2() As String = {
            "               ║║               ",
            "              ═╬╬═              ",
            "    ╬══════════║║══════════╬    ",
            "  ╬══════════════════════════╬  ",
            " ││    ╬═══════╬╬═══════╬    ││ ",
            "╬│    ╬        ║║        ╬    │╬",
            "║│   ╬         ║║         ╬   │║",
            "║│  ╬          ║║          ╬  │║",
            "║│ ╬║          ║║          ║╬ │║",
            "║  ║║     ╬    ║║    ╬     ║║  ║",
            "║  ║║     ║║   ║║   ║║     ║║  ║",
            "╬  ║╬     ║║║  ║║  ║║║     ╬║  ╬",
            "║  ╬║     ║║║  ║║  ║║║     ║╬  ║",
            "║  ║║     ║║║  ║║  ║║║     ║║  ║",
            "╬  ║║     ║║║  ║║  ║║║     ║║  ╬",
            "║│ ║║     ║║   ║║   ║║     ║║ │║",
            "║│ ║║     ╬    ║║    ╬     ║║ │║",
            "║│ ║║          ║║          ║║ │║",
            "║│ ║║          ║║          ║║ │║",
            "╬││════════════││════════════││╬",
            " │════════════│══│════════════│  "
                               }

        Dim str3() As String = {
            "                        ║║                        ",
            "                       ═╬╬═                       ",
            "             ╬══════════║║══════════╬             ",
            "           ╬══════════════════════════╬           ",
            "╬═══════╬ ││                          ││ ╬═══════╬",
            "║        ╬│                            │╬        ║",
            "║         ╬                            ╬         ║",
            "║          ╬                          ╬          ║",
            "║  │    │  ║╬                        ╬║  │    │  ║",
            "║  │ ╬╬ │  ║║                        ║║  │ ┼┼ │  ║",
            "║  │ ║║ │  ║║                        ║║  │ ││ │  ║",
            "║  │ ║║ │  ║║                        ║╬  │ ││ │  ║",
            "║  │ ║║ │  ║╬                        ╬║  │ ││ │  ║",
            "║  │ ║║ │  ║║                        ║║  │ ││ │  ║",
            "║  │ ╬╬ │  ║║                        ║║  │ ┼┼ │  ║",
            "║  │    │  ║║                        ║║  │    │  ║",
            "║          ║║                        ║║          ║",
            "║          ║║                        ║║          ║",
            "║          ║║                        ║║          ║",
            "│══════════╬║           ││           ║╬══════════│",
            "          │════════════│══│════════════│          "
                               }

        Dim str4() As String = {
            "╔════╗  ╔══╗  ╔╗╔╗  ╔═══╗  ╔═══╗  ╔╗─╔╗                           ╔══╗─  ╔╗╔╗  ╔╗─╔╗  ╔═══╗  ╔═══╗  ╔══╗  ╔╗─╔╗",
            "╚═╗╔═╝  ║╔╗║  ║║║║  ║╔══╝  ║╔═╗║  ║╚═╝║                           ║╔╗╚╗  ║║║║  ║╚═╝║  ║╔══╝  ║╔══╝  ║╔╗║  ║╚═╝║",
            "──║║──  ║╚╝║  ║║║║  ║╚══╗  ║╚═╝║  ║╔╗─║                           ║║╚╗║  ║║║║  ║╔╗─║  ║║╔═╗  ║╚══╗  ║║║║  ║╔╗─║",
            "──║║──  ║╔╗║  ║╚╝║  ║╔══╝  ║╔╗╔╝  ║║╚╗║                           ║║─║║  ║║║║  ║║╚╗║  ║║╚╗║  ║╔══╝  ║║║║  ║║╚╗║",
            "──║║──  ║║║║  ╚╗╔╝  ║╚══╗  ║║║║─  ║║─║║                           ║╚═╝║  ║╚╝║  ║║─║║  ║╚═╝║  ║╚══╗  ║╚╝║  ║║─║║",
            "──╚╝──  ╚╝╚╝  ─╚╝─  ╚═══╝  ╚╝╚╝─  ╚╝─╚╝                           ╚═══╝  ╚══╝  ╚╝─╚╝  ╚═══╝  ╚═══╝  ╚══╝  ╚╝─╚╝"
                               }

        Dim str5() As String = {
            "╔════╗      ╔══╗      ╔╗╔╗      ╔═══╗      ╔═══╗      ╔╗─╔╗",
            "╚═╗╔═╝      ║╔╗║      ║║║║      ║╔══╝      ║╔═╗║      ║╚═╝║",
            "──║║──      ║╚╝║      ║║║║      ║╚══╗      ║╚═╝║      ║╔╗─║",
            "──║║──      ║╔╗║      ║╚╝║      ║╔══╝      ║╔╗╔╝      ║║╚╗║",
            "──║║──      ║║║║      ╚╗╔╝      ║╚══╗      ║║║║─      ║║─║║",
            "──╚╝──      ╚╝╚╝      ─╚╝─      ╚═══╝      ╚╝╚╝─      ╚╝─╚╝"
                               }

        Dim str6() As String = {
            "╔══╗─     ╔╗╔╗     ╔╗─╔╗     ╔═══╗     ╔═══╗     ╔══╗     ╔╗─╔╗",
            "║╔╗╚╗     ║║║║     ║╚═╝║     ║╔══╝     ║╔══╝     ║╔╗║     ║╚═╝║",
            "║║╚╗║     ║║║║     ║╔╗─║     ║║╔═╗     ║╚══╗     ║║║║     ║╔╗─║",
            "║║─║║     ║║║║     ║║╚╗║     ║║╚╗║     ║╔══╝     ║║║║     ║║╚╗║",
            "║╚═╝║     ║╚╝║     ║║─║║     ║╚═╝║     ║╚══╗     ║╚╝║     ║║─║║",
            "╚═══╝     ╚══╝     ╚╝─╚╝     ╚═══╝     ╚═══╝     ╚══╝     ╚╝─╚╝"
                               }

        Dim menuItems() As String = {
            "TAVERN",
            "DUNGEON",
            "MAIN MENU"
                               }

        Dim l As Integer
        Dim symb As Char
        Dim str, userAns As String
        Dim pass As Boolean

        Do
            'Выводим изображение таверны
            Console.ForegroundColor = ConsoleColor.DarkCyan
            DisplayingPictureFromSide(str1, 20, 10, speed - 0.002)
            Console.ResetColor()

            'Выводим изображение закрытых ворот
            Console.ForegroundColor = ConsoleColor.Magenta
            DisplayingPictureFromSide(str2, 87, 7, speed - 0.002)
            Console.ResetColor()

            'Выводим |Tavern _ Dungeon|
            Console.WriteLine(Chr(10) & Chr(10))
            Console.ForegroundColor = ConsoleColor.Yellow
            If kingdom_of_snails Then
                ImageOutputLineByLine(str4, speed, 15)
            Else
                ImageOutputLineByLine(str4, speed + 0.08, 15)
            End If
            Console.ResetColor()

            'Вывод сообщения
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.SetCursorPosition(52, 2)
            str = "| WHERE DO YOU WANT TO GO? |"
            For i = 0 To str.Length - 1
                symb = str(i)
                Console.Write(symb)
                If Not kingdom_of_snails Then
                    Pause(0.05)
                End If

            Next
            Console.ResetColor()

            'Вывод пунктов меню выбора
            Console.SetCursorPosition(60, 6)
            l = 6
            For i = 0 To UBound(menuItems)
                Console.SetCursorPosition(60, l)

                If i <> 2 Then
                    Console.ForegroundColor = ConsoleColor.Cyan
                    Console.Write(i + 1 & "|" & " ")
                    Console.ResetColor()
                Else
                    Console.SetCursorPosition(60, l + 1)
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                    Console.Write(0 & "|" & " ")
                    Console.ResetColor()
                End If
                If i = 2 Then
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                End If
                str = menuItems(i)
                For j = 0 To menuItems(i).Length - 1
                    symb = str(j)
                    Console.Write(symb)
                    If Not kingdom_of_snails Then
                        Pause(0.05)
                    End If

                Next
                Console.ResetColor()
                l += 2

                If Not kingdom_of_snails Then
                    Pause(0.5)
                End If

            Next
            Console.SetCursorPosition(60, 15)

            str = "═════════════"
            For i = 0 To str.Length - 1
                symb = str(i)
                Console.Write(symb)
                If Not kingdom_of_snails Then
                    Pause(0.04)
                End If

            Next

            Console.SetCursorPosition(57, 14)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(userAns, 0, 2)

            If Not result Then
                pass = False
                speed = 0
                Console.SetCursorPosition(58, 17)
                str = "| INVALID VALUE |"
                Console.ForegroundColor = ConsoleColor.DarkRed
                For i = 0 To str.Length - 1
                    symb = str(i)
                    Console.Write(symb)
                    Pause(0.04)
                Next
                Console.ResetColor()

                Pause(1.1)
            Else
                pass = True
            End If

            Console.Clear()
        Loop Until pass

        Select Case userAns
            Case 1
                'Выводим изображение таверны | Золотой
                Console.ForegroundColor = ConsoleColor.DarkYellow
                DisplayingPictureFromSide(str1, 20, 10, speed - 0.003)
                Console.ResetColor()

                'Выводим |Tavern|
                Console.ForegroundColor = ConsoleColor.Yellow
                DisplayingPictureFromSide(str5, 68, 15, speed - 0.003)
                Console.ResetColor()

                Pause(2)
                'Переход в локацию: Tavern
                Tavern()
            Case 2
                'Выводим изображение открытых ворот
                Console.ForegroundColor = ConsoleColor.Magenta
                DisplayingPictureFromSide(str3, 78, 7, speed - 0.003)
                Console.ResetColor()

                'Выводим |Dungeon|
                Console.ForegroundColor = ConsoleColor.Yellow
                DisplayingPictureFromSide(str6, 8, 15, speed - 0.003)
                Console.ResetColor()

                Pause(2)
                'Переход в локацию: Dungeon
                DungeonMenuPath()
            Case 0
                'Переход в главное меню
                mainMenu()
        End Select

    End Sub
    'Окно выхода
    Sub ExitWindow()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.0035
        End If


        Dim str1() As String = {
            "║    ║                       ║    ║",
            "║    ║                       ║    ║",
            "║    ║                       ║    ║",
            "║    ║                       ║    ║",
            "║    ║                       ║    ║",
            "║    ║                       ║    ║",
            "║    ║                       ║    ║",
            "║    ║                       ║    ║",
            "║    ║                       ║    ║",
            "║    ╔═══╗─╔══╗╔══╗─╔══╗─╔════╗   ║",
            "║    ║╔══╝─╚═╗║║╔═╝─╚╗╔╝─╚═╗╔═╝   ║",
            "║    ║╚══╗───║╚╝║────║║────║║     ║",
            "║    ║╔══╝───║╔╗║────║║────║║     ║",
            "║    ║╚══╗─╔═╝║║╚═╗─╔╝╚╗───║║     ║",
            "║    ╚═══╝─╚══╝╚══╝─╚══╝───╚╝     ║",
            "║                                 ║",
            "╔╗╔╗╔╗─╔══╗─╔╗─╔╗─╔══╗──╔══╗─╔╗╔╗╔╗",
            "║║║║║║ ╚╗╔╝ ║╚═╝║ ║╔╗╚╗ ║╔╗║ ║║║║║║",
            "║║║║║║──║║──║╔╗─║ ║║╚╗║ ║║║║ ║║║║║║",
            "║║║║║║──║║──║║╚╗║ ║║─║║ ║║║║ ║║║║║║",
            "║╚╝╚╝║ ╔╝╚╗ ║║─║║ ║╚═╝║ ║╚╝║ ║╚╝╚╝║",
            "╚═╝╚═╝─╚══╝─╚╝─╚╝─╚═══╝─╚══╝─╚═╝╚═╝"
                               }

        Dim str2() As String = {
            "  ╬════════════════════╬",
            " ║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒═╬═▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒║▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒║▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒═╬═▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "║║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            " ║║▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒║",
            "  ╬══════│══════│══════╬"
                               }

        Dim str3() As String = {
        "                   ╬════════════════════╬",
        "║════════════════╬║║                    ║",
        "║                 ║║                    ║",
        "║                 ║║                    ║",
        "║                 ║║                    ║",
        "║                 ║║                    ║",
        "║        ╬        ║║                    ║",
        "║        ║        ║║                    ║",
        "║        ║        ║║                    ║",
        "║        ║        ║║                    ║",
        "║      ║ ║ ║      ║║                    ║",
        "║      ╬═╬═╬      ║║                    ║",
        "║      ╬═╬═╬      ║║                    ║",
        "║      ║ ║ ║      ║║                    ║",
        "║        ║        ║║                    ║",
        "║        ║        ║║                    ║",
        "║        ║        ║║                    ║",
        "║        ╬        ║║                    ║",
        "║                 ║║                    ║",
        "║                 ║║                    ║",
        "║                 ║║                    ║",
        "║                 ║║                    ║",
        "║                 ║║                    ║",
        "║                 ║║                    ║",
        "║                 ║║                    ║",
        "║════════════════╬║║                    ║",
        "                   ╬══════│══════│══════╬"
                               }

        Dim num1() As String = {
            " ╔╗ ",
            "╔╝║ ",
            "╚╗║ ",
            " ║║ ",
            " ║║ ",
            " ╚╝ "
                               }
        Dim num2() As String = {
            "╔══╗",
            "╚═╗║",
            "╔═╝║",
            "║╔═╝",
            "║╚═╗",
            "╚══╝"
                               }
        Dim num3() As String = {
            "╔══╗",
            "╚═╗║",
            "╔═╝║",
            "╚═╗║",
            "╔═╝║",
            "╚══╝"
                               }

        Dim num4() As String = {
            "╔╗╔╗",
            "║║║║",
            "║╚╝║",
            "╚═╗║",
            "  ║║",
            "  ╚╝"
                                }

        Dim num5() As String = {
            "╔══╗",
            "║╔═╝",
            "║╚═╗",
            "╚═╗║",
            "╔═╝║",
            "╚══╝"
                                }

        Dim dialog1() As String = {
            "YES.....",
            "I THOUGHT SO...",
            "*THIS TIME YOU GOT THE BOOK OF THE WHITE WORM*",
            "| BEAUTY - 47; |",
            "| SPELLS + 97; |"
                                  }

        Dim dialog2() As String = {
            "WHAT CAN I SAY HERE?",
            "I AM SO SORRY FOR YOU",
            "*THIS TIME YOU GOT A PINK SHARPENER*",
            "| AGILITY + 3; |",
            "| CHARISMA - 243; |"
                                  }

        Dim dialog3() As String = {
            "WHAT THE HELL?! HOW IT TURNED HERE...",
            "AGAIN HE INTERFEREED IN MY WORK...",
            "*YOU RECEIVED RUSTY DOOR KEY*",
            "| POWER + 100.000.000; |",
            "| KNOWLEDGE + 700.000.000.000.000; |"
                                  }

        Dim dialog4() As String = {
            "GREAT SHOPPING, MY FRIEND",
            "ALREADY DARKING, IT'S TIME HOME",
            "*YOU RECEIVED 7 KILOGRAMS OF POTATO*",
            "| SATIETY + 100; |",
            "| TIME - 57; |"
                                  }

        Dim dialog5() As String = {
            "WAKE UP... WAKE UP!",
            "THE CAVE WILL BE DESTROYED!",
            "*THIS TIME YOU SURVIVED*",
            "| AGILITY + 235; |",
            "| SPEED + 70; |"
                                  }
        Dim dialog6() As String = {
            "IT WASN'T WORTH DOING THIS",
            "THIS DOOR IS NOT SIMPLE",
            "*YOU HAVE BEEN REFUSED TO ACCESS THIS DOOR*",
            "| TIME - 900.000; |",
            "| INTELLIGENCE + 3; |"
                                  }

        Dim dialog7() As String = {
            "EVERYTHING IS AS USUAL",
            "I GIVE OUT - YOU COLLECT IT AND ENJOY",
            "*YOU HAVE GOT A LITTLE OF COINS AND SOME HAPPINESS*",
            "| COINS + 30; |",
            "| HAPPINESS + 17; |"
                                  }


        Dim dialogItems() As String = {
            "",
            "",
            "",
            "",
            ""
                                      }

        Dim menuItems() As String = {
            "DOOR #1",
            "DOOR #2",
            "DOOR #3",
            "RETURN TO MENU"
                                    }

        Dim l, rndNum As Integer
        Dim str, userAns As String
        Dim symb As Char
        Dim pass As Boolean

        Do
            'Вывод |Exit Window|
            If Not kingdom_of_snails Then
                Console.ForegroundColor = ConsoleColor.Cyan
                ImageOutputLineByLine(str1, speed + 0.018, 6)
                Console.ResetColor()
            Else
                Console.ForegroundColor = ConsoleColor.Cyan
                ImageOutputLineByLine(str1, speed, 6)
                Console.ResetColor()
            End If


            l = 0
            'Вывод полос справа
            For i = 1 To 39
                Console.SetCursorPosition(134, l)
                Console.ForegroundColor = ConsoleColor.Yellow
                Console.Write("║║")
                Console.ResetColor()
                Console.ForegroundColor = ConsoleColor.DarkCyan
                Console.Write("║║")
                Console.ResetColor()
                Console.ForegroundColor = ConsoleColor.Magenta
                Console.Write("║║")
                Console.ResetColor()
                l += 1

                If Not kingdom_of_snails Then
                    Pause(0.01)
                End If
            Next

            'Вывод изображений дверей
            Console.ForegroundColor = ConsoleColor.Yellow
            DisplayingPictureFromSide(str2, 47, 10, speed - 0.01)
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkCyan
            DisplayingPictureFromSide(str2, 75, 10, speed - 0.01)
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.Magenta
            DisplayingPictureFromSide(str2, 103, 10, speed - 0.01)
            Console.ResetColor()

            'Вывод нумерации дверей
            Console.ForegroundColor = ConsoleColor.Cyan
            For i = 0 To 2
                Select Case i
                    Case 0
                        If Not kingdom_of_snails Then
                            DisplayingPictureFromSide(num1, 59, 2, speed + 0.01)
                        Else
                            DisplayingPictureFromSide(num1, 59, 2, speed)
                        End If
                    Case 1
                        If Not kingdom_of_snails Then
                            DisplayingPictureFromSide(num2, 87, 2, speed + 0.01)
                        Else
                            DisplayingPictureFromSide(num2, 87, 2, speed)
                        End If

                    Case 2
                        If Not kingdom_of_snails Then
                            DisplayingPictureFromSide(num3, 115, 2, speed + 0.01)
                        Else
                            DisplayingPictureFromSide(num3, 115, 2, speed)
                        End If

                End Select
            Next
            Console.ResetColor()

            str = "WELCOME TO THE DOOR HALL"

            Console.SetCursorPosition(12, 23)
            Console.ForegroundColor = ConsoleColor.Yellow
            For i = 0 To str.Length - 1
                symb = str(i)
                Console.Write(symb)
                If Not kingdom_of_snails Then
                    Pause(0.05)
                End If

            Next
            Console.ResetColor()
            If Not kingdom_of_snails Then
                Pause(0.8)
            End If
            str = "| YOU FILL WITH DECISION |"

            Console.SetCursorPosition(11, 25)
            Console.ForegroundColor = ConsoleColor.Magenta
            For i = 0 To str.Length - 1
                symb = str(i)
                Console.Write(symb)
                If Not kingdom_of_snails Then
                    Pause(0.05)
                End If
            Next
            Console.ResetColor()
            If Not kingdom_of_snails Then
                Pause(0.4)
            End If

            str = "TIME TO MAKE A CHOICE..."

            Console.SetCursorPosition(12, 27)
            Console.ForegroundColor = ConsoleColor.Yellow
            For i = 0 To str.Length - 1
                symb = str(i)
                Console.Write(symb)
                If Not kingdom_of_snails Then
                    Pause(0.05)
                End If

            Next
            Console.ResetColor()

            'Вывод списка меню
            l = 30
            For i = 0 To UBound(menuItems)
                Console.SetCursorPosition(12, l)
                If i <> 3 Then
                    Console.ForegroundColor = ConsoleColor.Cyan
                    Console.Write(i + 1 & "|" & " ")
                    Console.ResetColor()
                Else
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                    Console.Write("0" & "|" & " ")
                    Console.ResetColor()
                End If
                str = menuItems(i)

                If i <> 3 Then
                    For j = 0 To menuItems(i).Length - 1
                        symb = str(j)
                        Console.Write(symb)
                        If Not kingdom_of_snails Then
                            Pause(0.05)
                        End If
                    Next
                Else
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                    For j = 0 To menuItems(i).Length - 1
                        symb = str(j)
                        Console.Write(symb)
                        If Not kingdom_of_snails Then
                            Pause(0.05)
                        End If
                    Next
                    Console.ResetColor()
                End If
                l += 2

                If Not kingdom_of_snails Then
                    Pause(0.5)
                End If
            Next

            Console.SetCursorPosition(12, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()
            result = CheckUserAns(userAns, 0, 3)

            If Not result Then
                pass = False
                speed = 0

                'Вывод |Exit Window| Красный
                Console.ForegroundColor = ConsoleColor.Red
                DisplayingPictureFromSide(str1, 6, 0, speed - 0.08)
                Console.ResetColor()

                'Нумерация дверей | красная
                Console.ForegroundColor = ConsoleColor.Red
                For i = 0 To 2
                    Select Case i
                        Case 0
                            DisplayingPictureFromSide(num1, 59, 2, speed + 0.01)
                        Case 1
                            DisplayingPictureFromSide(num2, 87, 2, speed + 0.01)
                        Case 2
                            DisplayingPictureFromSide(num3, 115, 2, speed + 0.01)
                    End Select
                Next
                Console.ResetColor()

                Pause(2)
                Console.Clear()
            Else
                pass = True
            End If
        Loop Until pass

        Console.Clear()
        If Not kingdom_of_snails Then
            speed = 0.0035
        Else
            speed = 0
        End If


        Select Case userAns
            Case 0
                'Возвращение в главное меню
                mainMenu()
                Exit Sub
            Case 1
                'Открытие 1-ой двери
                Console.ForegroundColor = ConsoleColor.Yellow
                DisplayingPictureFromSide(str2, 58, 2, speed - 0.002)
                Console.ResetColor()
                Pause(2.2)
                'Вывод открытой двери
                Console.ForegroundColor = ConsoleColor.Yellow
                DisplayingPictureFromSide(str3, 41, 2, speed)
                Console.ResetColor()
            Case 2
                'Открытие 2-ой двери
                Console.ForegroundColor = ConsoleColor.DarkCyan
                DisplayingPictureFromSide(str2, 58, 2, speed)
                Console.ResetColor()
                Pause(2.2)
                'Вывод открытой двери
                Console.ForegroundColor = ConsoleColor.DarkCyan
                DisplayingPictureFromSide(str3, 41, 2, speed)
                Console.ResetColor()
            Case 3
                'Открытие 3-ий двери
                Console.ForegroundColor = ConsoleColor.Magenta
                DisplayingPictureFromSide(str2, 58, 2, speed)
                Console.ResetColor()
                Pause(2.2)
                'Вывод открытой двери
                Console.ForegroundColor = ConsoleColor.Magenta
                DisplayingPictureFromSide(str3, 41, 2, speed)
                Console.ResetColor()
        End Select

        str = "simpleDialogWindow"
        'Вывод диалогового окна
        DisplayingPictureFromSide(DisplayingDialogBox(str), 41, 30, speed)

        str = "iconDoorEssence"
        'Вывод иконки персонажа
        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(IssuingAnIcon(str), 26, 30, speed)
        Console.ResetColor()

        'Вывод имени персонажа
        Console.SetCursorPosition(44, 31)
        str = "DOOR ESSENCE"
        Console.ForegroundColor = ConsoleColor.DarkYellow
        For i = 0 To str.Length - 1
            symb = str(i)
            Console.Write(symb)
            If Not kingdom_of_snails Then
                Pause(0.05)
            End If
        Next
        Console.ResetColor()

        'Выбор и вывод реплики
        rndNum = RandomNum(10, 1)

        Select Case rndNum
            Case 1
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog1(i)
                Next
            Case 2
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog2(i)
                Next
            Case 3
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog3(i)
                Next
            Case 4
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog4(i)
                Next
            Case 5
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog5(i)
                Next
            Case 6
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog6(i)
                Next
            Case 7
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog7(i)
                Next
            Case 8
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog7(i)
                Next
            Case 9
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog7(i)
                Next
            Case 10
                For i = 0 To UBound(dialogItems)
                    dialogItems(i) = dialog7(i)
                Next
        End Select

        l = 32
        For i = 0 To UBound(dialogItems)
            str = dialogItems(i)
            Console.SetCursorPosition(44, l)
            For j = 0 To dialogItems(i).Length - 1
                symb = str(j)
                Console.Write(symb)
                If Not kingdom_of_snails Then
                    Pause(0.05)
                End If
            Next
            l += 1

            If Not kingdom_of_snails Then
                Pause(0.5)
            End If
            Console.WriteLine()
        Next

        Pause(3)

        'Отсчёт от 5 до 1
        Console.ForegroundColor = ConsoleColor.Cyan
        For i = 1 To 5
            Select Case i
                Case 1
                    DisplayingPictureFromSide(num5, 70, 10, speed)
                Case 2
                    DisplayingPictureFromSide(num4, 70, 10, speed)
                Case 3
                    DisplayingPictureFromSide(num3, 70, 10, speed)
                Case 4
                    DisplayingPictureFromSide(num2, 70, 10, speed)
                Case 5
                    DisplayingPictureFromSide(num1, 70, 10, speed)
            End Select
            Pause(1)
        Next
        Console.ResetColor()

        End
    End Sub
    Sub Show_posters()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        If Not kingdom_of_snails Then
            speed = 0.0045
        Else
            speed = 0
        End If
        Dim pic_letter_p() As String =
            {
                "██████╗ ",
                "██╔══██╗",
                "██████╔╝",
                "██╔═══╝ ",
                "██║     ",
                "╚═╝     "
            }
        Dim pic_letter_o() As String =
            {
                " █████╗ ",
                "██╔══██╗",
                "██║  ██║",
                "██║  ██║",
                "╚█████╔╝",
                " ╚════╝ "
            }
        Dim pic_letter_s() As String =
            {
                " ██████╗",
                "██╔════╝",
                "╚█████╗ ",
                " ╚═══██╗",
                "██████╔╝",
                "╚═════╝ "
            }
        Dim pic_letter_t() As String =
            {
                "████████╗",
                "╚══██╔══╝",
                "   ██║   ",
                "   ██║   ",
                "   ██║   ",
                "   ╚═╝   "
            }
        Dim pic_letter_e() As String =
            {
                "███████╗",
                "██╔════╝",
                "█████╗  ",
                "██╔══╝  ",
                "███████╗",
                "╚══════╝"
            }
        Dim pic_letter_r() As String =
            {
                "██████╗ ",
                "██╔══██╗",
                "██████╔╝",
                "██╔══██╗",
                "██║  ██║",
                "╚═╝  ╚═╝"
            }

        Dim pic_poster_one() As String =
            {
                "╬═  ╔═══╦══╦══╦════╦═══╦═══╗──╔╗  ═╬",
                "║   ║╔═╗║╔╗║╔═╩═╗╔═╣╔══╣╔═╗║─╔╝║   ║",
                "║   ║╚═╝║║║║╚═╗─║║─║╚══╣╚═╝║─╚╗║   ║",
                "║   ║╔══╣║║╠═╗║─║║─║╔══╣╔╗╔╝──║║   ║",
                "║   ║║──║╚╝╠═╝║─║║─║╚══╣║║║───║║   ║",
                "╬═  ╚╝──╚══╩══╝─╚╝─╚═══╩╝╚╝───╚╝  ═╬"
            }

        Dim pic_poster_two() As String =
            {
                "╬═  ╔═══╦══╦══╦════╦═══╦═══╗╔══╗  ═╬",
                "║   ║╔═╗║╔╗║╔═╩═╗╔═╣╔══╣╔═╗║╚═╗║   ║",
                "║   ║╚═╝║║║║╚═╗─║║─║╚══╣╚═╝║╔═╝║   ║",
                "║   ║╔══╣║║╠═╗║─║║─║╔══╣╔╗╔╝║╔═╝   ║",
                "║   ║║──║╚╝╠═╝║─║║─║╚══╣║║║─║╚═╗   ║",
                "╬═  ╚╝──╚══╩══╝─╚╝─╚═══╩╝╚╝─╚══╝  ═╬"
            }

        Dim pic_poster_three() As String =
            {
                "╬═  ╔═══╦══╦══╦════╦═══╦═══╗╔══╗  ═╬",
                "║   ║╔═╗║╔╗║╔═╩═╗╔═╣╔══╣╔═╗║╚═╗║   ║",
                "║   ║╚═╝║║║║╚═╗─║║─║╚══╣╚═╝║╔═╝║   ║",
                "║   ║╔══╣║║╠═╗║─║║─║╔══╣╔╗╔╝╚═╗║   ║",
                "║   ║║──║╚╝╠═╝║─║║─║╚══╣║║║─╔═╝║   ║",
                "╬═  ╚╝──╚══╩══╝─╚╝─╚═══╩╝╚╝─╚══╝  ═╬"
            }

        Dim stick As String = "║║████████████████████████████████║║"

        Dim info_table() As String =
            {
                "[ ENTER POSTER NUMBER TO VIEW IT ]",
                "[ GRAY COLOR - MEANS THAT THE POSTER IS NOT RECEIVED ]",
                "[ PURPLE COLOR - MEANS POSTER RECEIVED ]",
                "[ ONLY RECEIVED POSTERS ARE OPENED FOR VIEWING ]",
                "[ PRESS ""ENTER"" TO RETURN FROM VIEWING THE POSTER ]",
                "[ ENTER 0 ""ZERO"" TO RETURN TO THE COLLECTION MENU ]"
            }

        Dim y As Integer
        Dim user_answer, str As String

        Do
            Console.SetCursorPosition(0, 2)
            Console.ForegroundColor = ConsoleColor.Yellow
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_letter_p, speed, 126)
                ImageOutputLineByLine(pic_letter_o, speed, 126)
                ImageOutputLineByLine(pic_letter_s, speed, 126)
                ImageOutputLineByLine(pic_letter_t, speed, 126)
                ImageOutputLineByLine(pic_letter_e, speed, 126)
                ImageOutputLineByLine(pic_letter_r, speed, 126)
            Else
                ImageOutputLineByLine(pic_letter_p, speed + 0.01, 126)
                ImageOutputLineByLine(pic_letter_o, speed + 0.01, 126)
                ImageOutputLineByLine(pic_letter_s, speed + 0.01, 126)
                ImageOutputLineByLine(pic_letter_t, speed + 0.01, 126)
                ImageOutputLineByLine(pic_letter_e, speed + 0.01, 126)
                ImageOutputLineByLine(pic_letter_r, speed + 0.01, 126)
            End If
            Console.ResetColor()
            Console.SetCursorPosition(0, 2)
            Console.ForegroundColor = ConsoleColor.DarkCyan
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_letter_p, speed, 118)
                ImageOutputLineByLine(pic_letter_o, speed, 118)
                ImageOutputLineByLine(pic_letter_s, speed, 118)
                ImageOutputLineByLine(pic_letter_t, speed, 118)
                ImageOutputLineByLine(pic_letter_e, speed, 118)
                ImageOutputLineByLine(pic_letter_r, speed, 118)
            Else
                ImageOutputLineByLine(pic_letter_p, speed + 0.01, 118)
                ImageOutputLineByLine(pic_letter_o, speed + 0.01, 118)
                ImageOutputLineByLine(pic_letter_s, speed + 0.01, 118)
                ImageOutputLineByLine(pic_letter_t, speed + 0.01, 118)
                ImageOutputLineByLine(pic_letter_e, speed + 0.01, 118)
                ImageOutputLineByLine(pic_letter_r, speed + 0.01, 118)
            End If
            Console.ResetColor()
            Console.SetCursorPosition(0, 2)
            Console.ForegroundColor = ConsoleColor.Magenta
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_letter_p, speed, 110)
                ImageOutputLineByLine(pic_letter_o, speed, 110)
                ImageOutputLineByLine(pic_letter_s, speed, 110)
                ImageOutputLineByLine(pic_letter_t, speed, 110)
                ImageOutputLineByLine(pic_letter_e, speed, 110)
                ImageOutputLineByLine(pic_letter_r, speed, 110)
            Else
                ImageOutputLineByLine(pic_letter_p, speed + 0.01, 110)
                ImageOutputLineByLine(pic_letter_o, speed + 0.01, 110)
                ImageOutputLineByLine(pic_letter_s, speed + 0.01, 110)
                ImageOutputLineByLine(pic_letter_t, speed + 0.01, 110)
                ImageOutputLineByLine(pic_letter_e, speed + 0.01, 110)
                ImageOutputLineByLine(pic_letter_r, speed + 0.01, 110)
            End If
            Console.ResetColor()

            If Not kingdom_of_snails Then
                Pause(1)
            End If


            Console.SetCursorPosition(0, 7)
            If doneChapters(0) Then
                Console.ForegroundColor = ConsoleColor.Magenta
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_poster_one, speed, 8)
            Else
                ImageOutputLineByLine(pic_poster_one, speed + 0.05, 8)
            End If

            Console.ResetColor()
            If Not kingdom_of_snails Then
                Pause(0.3)
            End If

            Console.SetCursorPosition(0, 14)
            If doneChapters(1) Then
                Console.ForegroundColor = ConsoleColor.Magenta
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_poster_two, speed, 8)
            Else
                ImageOutputLineByLine(pic_poster_two, speed + 0.05, 8)
            End If

            Console.ResetColor()
            If Not kingdom_of_snails Then
                Pause(0.3)
            End If

            Console.SetCursorPosition(0, 21)
            If doneChapters(2) Then
                Console.ForegroundColor = ConsoleColor.Magenta
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
            End If
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_poster_three, speed, 8)
            Else
                ImageOutputLineByLine(pic_poster_three, speed + 0.05, 8)
            End If

            Console.ResetColor()

            y = 1
            For i = 0 To 4
                Console.SetCursorPosition(8, y)
                Console.WriteLine(stick)
                y += 1
                If Not kingdom_of_snails Then
                    Pause(0.02)
                End If

            Next

            y = 28
            For i = 0 To 10
                Console.SetCursorPosition(8, y)
                Console.WriteLine(stick)
                y += 1
                If Not kingdom_of_snails Then
                    Pause(0.02)
                End If
            Next

            y = 7
            Console.ForegroundColor = ConsoleColor.Yellow
            For i = 0 To UBound(info_table)
                Console.SetCursorPosition(50, y)
                Console.WriteLine(info_table(i))
                y += 2
            Next
            Console.ResetColor()

            Console.SetCursorPosition(50, 27)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_answer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_answer, 0, 3)

            If result Then
                Select Case user_answer
                    Case "0"
                        'Переход в меню коллекции игрока
                        user_collection()
                    Case "1"
                        If doneChapters(0) Then
                            Poster_prophecy_the_dungeon_silent_temple(1)
                        Else
                            str = "[ INVALID VALUE ]"
                            LineOutPutCharacterByCharacter(str, 50, 27, speed, ConsoleColor.DarkRed)

                            Pause(2)
                            Console.Clear()
                        End If
                    Case "2"
                        If doneChapters(1) Then
                            Poster_ancient_evil_is_invincible(1)
                        Else
                            str = "[ INVALID VALUE ]"
                            LineOutPutCharacterByCharacter(str, 50, 27, speed, ConsoleColor.DarkRed)

                            Pause(2)
                            Console.Clear()
                        End If
                    Case "3"
                        If doneChapters(2) Then
                            Poster_accept_your_destiny(1)
                        Else
                            str = "[ INVALID VALUE ]"
                            LineOutPutCharacterByCharacter(str, 50, 27, speed, ConsoleColor.DarkRed)

                            Pause(2)
                            Console.Clear()
                        End If
                End Select
            Else
                str = "[ INVALID VALUE ]"
                LineOutPutCharacterByCharacter(str, 50, 27, speed, ConsoleColor.DarkRed)

                Pause(2)
                Console.Clear()
            End If
        Loop

    End Sub
    Sub User_collection()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.0045
        End If


        Dim pic_letters_coll() As String =
            {
                " █████╗  █████╗ ██╗     ██╗     ",
                "██╔══██╗██╔══██╗██║     ██║     ",
                "██║  ╚═╝██║  ██║██║     ██║     ",
                "██║  ██╗██║  ██║██║     ██║     ",
                "╚█████╔╝╚█████╔╝███████╗███████╗",
                " ╚════╝  ╚════╝ ╚══════╝╚══════╝"
            }

        Dim pic_letter_e() As String =
            {
                "███████╗",
                "██╔════╝",
                "█████╗  ",
                "██╔══╝  ",
                "███████╗",
                "╚══════╝"
            }

        Dim pic_letters_tion() As String =
            {
                "████████╗██╗ █████╗ ███╗  ██╗",
                "╚══██╔══╝██║██╔══██╗████╗ ██║",
                "   ██║   ██║██║  ██║██╔██╗██║",
                "   ██║   ██║██║  ██║██║╚████║",
                "   ██║   ██║╚█████╔╝██║ ╚███║",
                "   ╚═╝   ╚═╝ ╚════╝ ╚═╝  ╚══╝"
            }

        Dim pic_border_posters() As String =
            {
                "═════════════════║        ║═════ ",
                "║                               ║",
                "║  ╔═══╦══╦══╦════╦═══╦═══╦══╗  ║",
                "║  ║╔═╗║╔╗║╔═╩═╗╔═╣╔══╣╔═╗║╔═╝  ║",
                "║  ║╚═╝║║║║╚═╗─║║─║╚══╣╚═╝║╚═╗  ║",
                "║  ║╔══╣║║╠═╗║─║║─║╔══╣╔╗╔╩═╗║  ║",
                "║  ║║──║╚╝╠═╝║─║║─║╚══╣║║║╔═╝║  ║",
                "║  ╚╝──╚══╩══╝─╚╝─╚═══╩╝╚╝╚══╝  ║",
                "║                               ║",
                " ═════║        ║═════════════════"
            }

        Dim pic_border_collectibles() As String =
            {
                "═════════════════║                       ║═════ ",
                "║                                              ║",
                "║  ╔══╦══╦╗─╔╗─╔═══╦══╦════╦══╦══╗╔╗─╔═══╦══╗  ║",
                "║  ║╔═╣╔╗║║─║║─║╔══╣╔═╩═╗╔═╩╗╔╣╔╗║║║─║╔══╣╔═╝  ║",
                "║  ║║─║║║║║─║║─║╚══╣║───║║──║║║╚╝╚╣║─║╚══╣╚═╗  ║",
                "║  ║║─║║║║║─║║─║╔══╣║───║║──║║║╔═╗║║─║╔══╩═╗║  ║",
                "║  ║╚═╣╚╝║╚═╣╚═╣╚══╣╚═╗─║║─╔╝╚╣╚═╝║╚═╣╚══╦═╝║  ║",
                "║  ╚══╩══╩══╩══╩═══╩══╝─╚╝─╚══╩═══╩══╩═══╩══╝  ║",
                "║                                              ║",
                " ═════║                       ║════════════════ "
            }

        Dim pic_posters() As String =
            {
                "╔═══╦══╦══╦════╦═══╦═══╦══╗",
                "║╔═╗║╔╗║╔═╩═╗╔═╣╔══╣╔═╗║╔═╝",
                "║╚═╝║║║║╚═╗─║║─║╚══╣╚═╝║╚═╗",
                "║╔══╣║║╠═╗║─║║─║╔══╣╔╗╔╩═╗║",
                "║║──║╚╝╠═╝║─║║─║╚══╣║║║╔═╝║",
                "╚╝──╚══╩══╝─╚╝─╚═══╩╝╚╝╚══╝"
            }

        Dim pic_collectibles() As String =
            {
                "╔══╦══╦╗─╔╗─╔═══╦══╦════╦══╦══╗╔╗─╔═══╦══╗",
                "║╔═╣╔╗║║─║║─║╔══╣╔═╩═╗╔═╩╗╔╣╔╗║║║─║╔══╣╔═╝",
                "║║─║║║║║─║║─║╚══╣║───║║──║║║╚╝╚╣║─║╚══╣╚═╗",
                "║║─║║║║║─║║─║╔══╣║───║║──║║║╔═╗║║─║╔══╩═╗║",
                "║╚═╣╚╝║╚═╣╚═╣╚══╣╚═╗─║║─╔╝╚╣╚═╝║╚═╣╚══╦═╝║",
                "╚══╩══╩══╩══╩═══╩══╝─╚╝─╚══╩═══╩══╩═══╩══╝"
            }

        Dim stick() As String =
            {
                "═",
                "█",
                "█",
                "█",
                "█",
                "═"
            }

        Dim info_table() As String =
            {
                "[ ENTER 1 ""ONE"" TO GO TO VIEWING POSTERS ]",
                "[ ENTER 0 ""ZERO"" TO RETURN TO THE MAIN MENU ]"
            }

        Dim user_answer, str As String

        Do
            DisplayingPictureFromSide(pic_letters_coll, 55, 5, speed - 0.001)
            DisplayingPictureFromSide(pic_letter_e, 87, 5, speed - 0.001)
            DisplayingPictureFromSide(pic_letters_tion, 95, 5, speed - 0.001)
            Console.ResetColor()

            For i = 0 To 52
                For j = 0 To UBound(stick)
                    Console.SetCursorPosition(1 + i, 5 + j)
                    Console.Write(stick(j))
                    If Not kingdom_of_snails Then
                        Pause(0.003)
                    End If
                Next
            Next

            For i = 0 To 14
                For j = 0 To UBound(stick)
                    Console.SetCursorPosition(125 + i, 5 + j)
                    Console.Write(stick(j))
                    If Not kingdom_of_snails Then
                        Pause(0.003)
                    End If
                Next
            Next

            If Not kingdom_of_snails Then
                Pause(0.5)
            End If

            DisplayingPictureFromSide(pic_border_posters, 13, 19, speed - 0.001)
            If Not kingdom_of_snails Then
                Pause(0.5)
            End If

            DisplayingPictureFromSide(pic_border_collectibles, 59, 19, speed - 0.001)

            Console.ForegroundColor = ConsoleColor.DarkMagenta
            DisplayingPictureFromSide(pic_posters, 16, 21, speed - 0.001)
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkGray
            DisplayingPictureFromSide(pic_collectibles, 62, 21, speed - 0.001)
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.Yellow
            Console.SetCursorPosition(13, 35)
            Console.WriteLine(info_table(0))
            Console.SetCursorPosition(13, 37)
            Console.WriteLine(info_table(1))
            Console.ResetColor()

            Console.SetCursorPosition(70, 37)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_answer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_answer, 0, 1)

            If Not result Then
                str = "[ INVALID VALUE ]"
                LineOutPutCharacterByCharacter(str, 70, 37, speed + 0.001, ConsoleColor.Red)
                Pause(2)
                Console.Clear()
            Else
                Select Case user_answer
                    Case "0"
                        'Переход в главное меню
                        mainMenu()
                    Case "1"
                        'Переход в просмотр постеров
                        Show_posters()
                End Select
            End If
        Loop

    End Sub
    Sub Game_settings()
        Console.WindowHeight = 40
        Console.WindowWidth = 100
        Console.SetBufferSize(100, 40)
        Console.Clear()

        If kingdom_of_snails Then
            speed = 0
        End If

        Dim pic_fix_mode() As String =
            {
                "███████╗██╗██╗  ██╗  ███╗   ███╗ █████╗ ██████╗ ███████╗",
                "██╔════╝██║╚██╗██╔╝  ████╗ ████║██╔══██╗██╔══██╗██╔════╝",
                "█████╗  ██║ ╚███╔╝   ██╔████╔██║██║  ██║██║  ██║█████╗  ",
                "██╔══╝  ██║ ██╔██╗   ██║╚██╔╝██║██║  ██║██║  ██║██╔══╝  ",
                "██║     ██║██╔╝╚██╗  ██║ ╚═╝ ██║╚█████╔╝██████╔╝███████╗",
                "╚═╝     ╚═╝╚═╝  ╚═╝  ╚═╝     ╚═╝ ╚════╝ ╚═════╝ ╚══════╝"
            }

        Dim info_table() As String =
            {
                "[ TO GET 100 COINS ENTER THE COMMAND - GREEDISGOOD ]",
                "[ LIMITATION ON THE QUANTITY OF COINS - 9999999999999 C ]",
                "[ TO OPEN ONE THE CHAPTER ENTER THE COMMAND - ALLYOURBASEAREBELONGTOUS ]",
                "[ USE OF OPENING GAME CHAPTERS LEADS TO CONSEQUENCES ]",
                "[ ENTER | KINGDOMOFSNAILS | TO TURN ON AND OFF ANIMATION MODE ]",
                "[ CLEAR YOUR SAVINGS | RESTART THE APPLICATION - GIVEMEFREEDOM ]",
                "[ TO EXIT THE FIX MODE ENTER 0 ""ZERO"" ]",
                "[ ALL FIX MODE ACTIONS ARE CONTROLLED BY ANTI CHEAT ZIGGURAT SYSTEM ]"
            }

        Dim y, w As Integer
        Dim str, user_answer As String

        Console.ForegroundColor = ConsoleColor.Green

        Do
            Console.ForegroundColor = ConsoleColor.Green

            Console.SetCursorPosition(0, 3)
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_fix_mode, speed, 9)
            Else
                ImageOutputLineByLine(pic_fix_mode, speed + 0.07, 9)
            End If

            y = 11
            For i = 0 To UBound(info_table)
                str = info_table(i)
                If kingdom_of_snails Then
                    LineOutPutCharacterByCharacter(str, 9, y, speed, ConsoleColor.Green)
                Else
                    LineOutPutCharacterByCharacter(str, 9, y, speed + 0.015, ConsoleColor.Green)
                End If

                y += 2
            Next

            Console.SetCursorPosition(9, y + 7)
            Console.ForegroundColor = ConsoleColor.Green
            Console.Write("-> ")
            user_answer = Console.ReadLine()
            Console.ResetColor()

            Select Case user_answer.ToUpper
                Case "ALLYOURBASEAREBELONGTOUS"
                    For i = 0 To UBound(doneChapters)
                        If Not doneChapters(i) Then
                            doneChapters(i) = True
                            Select Case i
                                Case 0
                                    For j = 0 To UBound(doneLevelsFromCharpet1)
                                        doneLevelsFromCharpet1(j) = True
                                    Next
                                Case 1
                                    For j = 0 To UBound(doneLevelsFromCharpet2)
                                        doneLevelsFromCharpet1(j) = True
                                    Next
                                Case 2
                                    For j = 0 To UBound(doneLevelsFromCharpet3)
                                        doneLevelsFromCharpet1(j) = True
                                    Next
                            End Select
                        End If
                    Next

                    w = 64
                    For k = 0 To 2
                        Console.SetCursorPosition(w, y + 3)
                        Console.ForegroundColor = ConsoleColor.Green
                        For i = 0 To 6
                            Console.SetCursorPosition(w, y + 3 + i)
                            Console.WriteLine("/")
                            If Not kingdom_of_snails Then
                                Pause(0.2)
                            End If

                        Next
                        Console.ResetColor()
                        w += 4
                    Next

                    Console.ForegroundColor = ConsoleColor.Green
                    Console.SetCursorPosition(9, y + 9)
                    Console.WriteLine("// DONE")
                    Console.ResetColor()

                    Pause(2)

                    Game_settings()
                Case "GREEDISGOOD"
                    playerCoins += 100

                    w = 64
                    For k = 0 To 2
                        Console.SetCursorPosition(w, y + 4)
                        Console.ForegroundColor = ConsoleColor.Green
                        For i = 0 To 6
                            Console.SetCursorPosition(w, y + 4 + i)
                            Console.WriteLine("/")
                            If Not kingdom_of_snails Then
                                Pause(0.2)
                            End If
                        Next
                        Console.ResetColor()
                        w += 4
                    Next

                    Console.ForegroundColor = ConsoleColor.Green
                    Console.SetCursorPosition(9, y + 9)
                    Console.WriteLine("// DONE")
                    Console.ResetColor()

                    Pause(2)

                    Game_settings()
                Case "GIVEMEFREEDOM"
                    Dim h As Integer

                    FileOpen(1, "save.txt", OpenMode.Output)

                    Do
                        str = save_param(h)

                        Select Case h
                            Case 0
                                str = "25"
                                Check_tasks_for_complete()
                            Case 1
                                For j = 0 To UBound(playerInventory)
                                    str &= "NULL"
                                    playerInventory(j) = False
                                    playerBag = 0
                                    If Not j = UBound(playerInventory) Then
                                        str &= ","
                                    End If
                                    Select Case playerInventoryNames(j)
                                        Case "BLUE SCARF"
                                            maxPlayerXp -= 20
                                        Case "CHAIN MAIL"
                                            maxPlayerXp -= 25
                                        Case "LAMB SWEATER"
                                            maxPlayerXp -= 15
                                        Case "GREEN SHORTS"
                                            maxPlayerXp -= 5
                                        Case "WOODEN SWORD"
                                            maxPlayerXp -= 5
                                        Case "BINOCULARS"
                                            maxPlayerXp -= 10
                                        Case "IRON SWORD"
                                            maxPlayerXp -= 10
                                        Case "DRUID'S STAFF"
                                            maxPlayerXp -= 50
                                        Case "BOOK OF THE DAMNED"
                                            maxPlayerXp -= 75
                                        Case "CHOCOLATE MARIO MUSHROOM"
                                            maxPlayerXp -= 30
                                    End Select
                                Next
                                Check_tasks_for_complete()
                            Case 2
                                For j = 0 To UBound(doneChapters)
                                    str &= "0"
                                    If Not j = UBound(doneChapters) Then
                                        str &= ","
                                    End If
                                Next
                                Check_tasks_for_complete()
                            Case 3
                                For j = 0 To UBound(doneLevelsFromCharpet1)
                                    str &= "0"
                                    If Not j = UBound(doneLevelsFromCharpet1) Then
                                        str &= ","
                                    End If
                                Next
                                Check_tasks_for_complete()
                            Case 4
                                For j = 0 To UBound(doneLevelsFromCharpet2)
                                    str &= "0"
                                    If Not j = UBound(doneLevelsFromCharpet2) Then
                                        str &= ","
                                    End If
                                Next
                                Check_tasks_for_complete()
                            Case 5
                                For j = 0 To UBound(doneLevelsFromCharpet3)
                                    str &= "0"
                                    If Not j = UBound(doneLevelsFromCharpet3) Then
                                        str &= ","
                                    End If
                                Next
                                Check_tasks_for_complete()
                            Case 6
                                For j = 0 To UBound(done_first_season_tasks)
                                    str &= "0"
                                    If Not j = UBound(done_first_season_tasks) Then
                                        str &= ","
                                    End If
                                Next
                                Check_tasks_for_complete()
                        End Select

                        PrintLine(1, str)

                        h += 1
                    Loop Until h = 6
                    FileClose(1)

                    Check_tasks_for_complete()

                    Console.ForegroundColor = ConsoleColor.Green
                    Console.SetCursorPosition(9, y + 9)
                    Console.WriteLine("// DONE")
                    Console.ResetColor()

                    Pause(2)

                    Game_settings()
                Case "KINGDOMOFSNAILS"
                    If kingdom_of_snails Then
                        kingdom_of_snails = False
                    Else
                        kingdom_of_snails = True
                    End If

                    Console.ForegroundColor = ConsoleColor.Green
                    Console.SetCursorPosition(9, y + 9)
                    Console.WriteLine("// DONE")
                    Console.ResetColor()
                    Check_tasks_for_complete()
                    Pause(2)
                    Console.Clear()
                Case "0"
                    Check_tasks_for_complete()
                    MainMenu()
                Case Else
                    str = "[ INVALID VALUE ]"
                    LineOutPutCharacterByCharacter(str, 9, y + 7, speed + 0.01, ConsoleColor.Green)
                    Pause(1)
                    Console.Clear()
            End Select
        Loop

        Console.ResetColor()
    End Sub
    Sub Game_settings_first_page()
        Console.WindowHeight = 40
        Console.WindowWidth = 100
        Console.SetBufferSize(100, 40)
        Console.Clear()

        If kingdom_of_snails Then
            speed = 0
        End If

        Dim pic_recovery() As String =
            {
                "██████╗ ███████╗ █████╗  █████╗ ██╗   ██╗███████╗██████╗ ██╗   ██╗",
                "██╔══██╗██╔════╝██╔══██╗██╔══██╗██║   ██║██╔════╝██╔══██╗╚██╗ ██╔╝",
                "██████╔╝█████╗  ██║  ╚═╝██║  ██║╚██╗ ██╔╝█████╗  ██████╔╝ ╚████╔╝ ",
                "██╔══██╗██╔══╝  ██║  ██╗██║  ██║ ╚████╔╝ ██╔══╝  ██╔══██╗  ╚██╔╝  ",
                "██║  ██║███████╗╚█████╔╝╚█████╔╝  ╚██╔╝  ███████╗██║  ██║   ██║   ",
                "╚═╝  ╚═╝╚══════╝ ╚════╝  ╚════╝    ╚═╝   ╚══════╝╚═╝  ╚═╝   ╚═╝   "
            }

        Dim pic_mode() As String =
            {
                "███╗   ███╗ █████╗ ██████╗ ███████╗",
                "████╗ ████║██╔══██╗██╔══██╗██╔════╝",
                "██╔████╔██║██║  ██║██║  ██║█████╗  ",
                "██║╚██╔╝██║██║  ██║██║  ██║██╔══╝  ",
                "██║ ╚═╝ ██║╚█████╔╝██████╔╝███████╗",
                "╚═╝     ╚═╝ ╚════╝ ╚═════╝ ╚══════╝"
            }

        Dim program_action() As String =
            {
                "[ LAUNCHING THE PROTOCOL ]",
                "[ ACCESS LEVEL CHECK ]",
                "[ PREPARATION OF THE ACCESS KEY ]",
                "[ USING THE ACCESS KEY ]",
                "[ COMPLETION OF THE PROTOCOL ]"
            }

        Dim y, l, z As Integer

        Console.ForegroundColor = ConsoleColor.Green

        Console.SetCursorPosition(0, 3)
        If kingdom_of_snails Then
            ImageOutputLineByLine(pic_recovery, speed, 9)
        Else
            ImageOutputLineByLine(pic_recovery, speed + 0.07, 9)
        End If

        Console.SetCursorPosition(0, 10)
        If kingdom_of_snails Then
            ImageOutputLineByLine(pic_mode, speed, 9)
        Else
            ImageOutputLineByLine(pic_mode, speed + 0.07, 9)
        End If


        l = 19
        y = 70
        For i = 0 To UBound(program_action)
            z = 19
            Console.SetCursorPosition(9, l)
            Console.WriteLine(program_action(i))
            If Not kingdom_of_snails Then
                Pause(1)
            End If


            For j = 0 To 4
                Console.SetCursorPosition(y, z)
                Console.WriteLine("/")
                z += 2
                If Not kingdom_of_snails Then
                    Pause(0.03)
                End If
            Next
            If Not kingdom_of_snails Then
                Pause(1)
            End If
            Console.SetCursorPosition(9, l + 2)
            Console.WriteLine("// DONE")

            l += 4
            y += 3
            If Not kingdom_of_snails Then
                Pause(0.5)
            End If
        Next

        Console.ResetColor()
        If Not kingdom_of_snails Then
            Pause(1)
        End If

        Pause(2.5)
        Game_settings()
    End Sub
    Sub Game_information_part_three_second_page()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0045

        Dim pic_hop_hop_ind() As String = {
            "██╗░░██╗░█████╗░██████╗░░░░░░░██╗░░██╗░█████╗░██████╗░░░░██╗███╗░░██╗░█████╗░",
            "██║░░██║██╔══██╗██╔══██╗░░░░░░██║░░██║██╔══██╗██╔══██╗░░░██║████╗░██║██╔══██╗",
            "███████║██║░░██║██████╔╝█████╗███████║██║░░██║██████╔╝░░░██║██╔██╗██║██║░░╚═╝",
            "██╔══██║██║░░██║██╔═══╝░╚════╝██╔══██║██║░░██║██╔═══╝░░░░██║██║╚████║██║░░██╗",
            "██║░░██║╚█████╔╝██║░░░░░░░░░░░██║░░██║╚█████╔╝██║░░░░░██╗██║██║░╚███║╚█████╔╝",
            "╚═╝░░╚═╝░╚════╝░╚═╝░░░░░░░░░░░╚═╝░░╚═╝░╚════╝░╚═╝░░░░░╚═╝╚═╝╚═╝░░╚══╝░╚════╝░"
                               }

        Dim stick() As String =
            {
                "═",
                "█",
                "█",
                "█",
                "█",
                "═"
            }

        Dim text_studio_information() As String =
            {
                "HOP-HOP.IND IS A TEAM OF FRIENDS THAT DEVELOPING IN DIFFERENT AREAS OF DIRECTIONS.",
                "THIS PROJECT-DIRECTION IN THE TEAM IS DEVELOPED BY THE STUDIO MEMBER - ARVEKS VEDEN.",
                "TOTAL IN THE TEAM 3 PARTICIPANTS: IVAN OLEСHOV, NIKOLAY DEGTEREV, ARTEM VEDENEEV.",
                "WE TRY TO LEARN NEW AND INTERESTING IN THE INFORMATION INDUSTRY.",
                "ONE SIDE THIS IS A MEETING OF PEOPLE WITH AN INTEREST IN EDUCATION AND A PROJECT",
                "WHICH WAS CREATED AS A JOKE PROJECT, BUT WHO KNOWS MAYBE SOMETHING BIGGER IN THE FUTURE.",
                "WE ARE VERY IMPORTANT YOUR EVALUATION OF OUR PRODUCTS,",
                "THEREFORE WRITE YOUR IMPRESSIONS TO THE OFFICIAL MAIL OF OUR STUDIO:",
                "| HOPHOP.INC.OFFICIAL@GMAIL.COM |. WE WILL HAPPY TO VIEW ALL YOUR REVIEWS."
            }

        Dim x_coord, y_coord As Byte
        Dim str As String

        Console.SetCursorPosition(0, 4)
        ImageOutputLineByLine(pic_hop_hop_ind, speed + 0.1, 34)

        x_coord = 2
        For i = 0 To 29
            DisplayingPictureFromSide(stick, x_coord, 4, 0.003)

            x_coord += 1
        Next

        x_coord = 112
        For i = 0 To 26
            DisplayingPictureFromSide(stick, x_coord, 4, 0.003)

            x_coord += 1
        Next

        x_coord = 30
        y_coord = 14
        For i = 0 To UBound(text_studio_information)
            str = text_studio_information(i)
            LineOutPutCharacterByCharacter(str, x_coord, y_coord, speed, ConsoleColor.Yellow)

            y_coord += 2
        Next

        Pause(1)

        str = "[ PRESS ANY KEY TO RETURN TO THE MENU ]"
        LineOutPutCharacterByCharacter(str, 30, 35, speed, ConsoleColor.Cyan)

        Console.ReadKey()
        game_information()
    End Sub
    Sub Game_information_part_three()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0045

        Dim pic_forerunners() As String =
            {
                "███████╗     █████╗     ██████╗     ███████╗    ██████╗     ██╗   ██╗    ███╗  ██╗    ███╗  ██╗    ███████╗    ██████╗      ██████╗",
                "██╔════╝    ██╔══██╗    ██╔══██╗    ██╔════╝    ██╔══██╗    ██║   ██║    ████╗ ██║    ████╗ ██║    ██╔════╝    ██╔══██╗    ██╔════╝",
                "█████╗      ██║  ██║    ██████╔╝    █████╗      ██████╔╝    ██║   ██║    ██╔██╗██║    ██╔██╗██║    █████╗      ██████╔╝    ╚█████╗ ",
                "██╔══╝      ██║  ██║    ██╔══██╗    ██╔══╝      ██╔══██╗    ██║   ██║    ██║╚████║    ██║╚████║    ██╔══╝      ██╔══██╗     ╚═══██╗",
                "██║         ╚█████╔╝    ██║  ██║    ███████╗    ██║  ██║    ╚██████╔╝    ██║ ╚███║    ██║ ╚███║    ███████╗    ██║  ██║    ██████╔╝",
                "╚═╝          ╚════╝     ╚═╝  ╚═╝    ╚══════╝    ╚═╝  ╚═╝     ╚═════╝     ╚═╝  ╚══╝    ╚═╝  ╚══╝    ╚══════╝    ╚═╝  ╚═╝    ╚═════╝ "
            }

        Dim coord_x, coord_k As Integer

        Console.SetCursorPosition(0, 17)
        Console.ForegroundColor = ConsoleColor.Cyan
        ImageOutputLineByLine(pic_forerunners, speed + 0.1, 6)
        Console.ResetColor()

        coord_x = 38
        coord_k = 103
        For i = 0 To 65
            Console.SetCursorPosition(coord_x, 11)
            Console.Write("─")
            Console.SetCursorPosition(coord_k, 27)
            Console.Write("─")
            Pause(0.01)
            coord_x += 1
            coord_k -= 1
        Next


        Pause(2)
        Console.Clear()
        Game_information_part_three_second_page()
    End Sub
    Sub Game_information_part_two()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0045

        Dim pic_future() As String =
            {
                "╔══╦╗╔╦════╦╗╔╦═══╦═══╗",
                "║╔═╣║║╠═╗╔═╣║║║╔═╗║╔══╝",
                "║╚═╣║║║─║║─║║║║╚═╝║╚══╗",
                "║╔═╣║║║─║║─║║║║╔╗╔╣╔══╝",
                "║║─║╚╝║─║║─║╚╝║║║║║╚══╗",
                "╚╝─╚══╝─╚╝─╚══╩╝╚╝╚═══╝"
            }
        Dim pic_info() As String =
            {
                "╔══╦╗─╔╦══╦══╗",
                "╚╗╔╣╚═╝║╔═╣╔╗║",
                "─║║║╔╗─║╚═╣║║║",
                "─║║║║╚╗║╔═╣║║║",
                "╔╝╚╣║─║║║─║╚╝║",
                "╚══╩╝─╚╩╝─╚══╝"
            }

        Dim information_text_one() As String =
            {
                "[ THE FUTURE OF THE GAME ]",
                "- THE GAME WILL BE DEVELOPED FOLLOWING THE DEVELOPMENT PLAN",
                "- THE DEVELOPMENT PLAN WILL BE PROVIDED LATER ON THE OFFICIAL",
                "  SITE OF THE GAME",
                "- IN PLANS ONLY 3 VERSIONS OF THE GAME.",
                "  EACH OF WHICH WILL BE AVAILABLE ON THE",
                "  OFFICIAL SITE OF THE GAME.",
                "- IT IS POSSIBLE NOT FAST WORK FROM THE STUDIO",
                "  ON THE DEVELOPMENT OF THE GAME",
                "  BECAUSE THE PROJECT IS CREATED AT THE MOMENT BY",
                "  ONE MEMBER OF THE",
                "  STUDIO HOP-HOP.INC. SIMULTANEOUSLY WITH",
                "  THIS STUDIO PARTICIPANT",
                "  IS TRAINING, SO TIME IS SPENDING FOR OTHER DIRECTIONS."
            }

        Dim information_text_two() As String =
            {
                "[ GAME VERSION INFORMATION ]",
               "- VERSION 1: RELEASED. | YOUR GAME VERSION MEETS 1ST |",
               "- VERSION 2: NOT RELEASED | PLANS - COORDINALLY",
               "  UPDATE GAME SYSTEM",
               "  & ADD GAME CONTENT |",
               "- VERSION 3: Not RELEASED | REFINING ALL GAME SYSTEMS",
               "  & ADDING MULTIPLE",
               "  GAME SEASONS WITH ADDITIONAL CONTENT |",
               "[ THE DEVELOPMENT PLAN WILL BE SUBMITTED ON THE",
               "  OFFICIAL WEBSITE OF THE GAME. ]"
            }

        Dim stick, str, user_asnwer As String
        Dim x_coord, y_coord As Integer
        stick = "║|"

        Do
            Console.ForegroundColor = ConsoleColor.DarkGray
            For i = 2 To 38
                Console.SetCursorPosition(69, i)
                Console.WriteLine(stick)
            Next
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(pic_future, 8, 2, speed - 0.001)
            DisplayingPictureFromSide(pic_info, 79, 2, speed - 0.001)
            Console.ResetColor()

            x_coord = 4
            y_coord = 11
            For i = 0 To UBound(information_text_one)
                Console.SetCursorPosition(x_coord, y_coord)
                If i = 0 Then
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                Else
                    Console.ForegroundColor = ConsoleColor.Yellow
                End If
                Console.WriteLine(information_text_one(i))
                y_coord += 2
                Console.ResetColor()
            Next

            x_coord = 75
            y_coord = 11
            For i = 0 To UBound(information_text_two)
                Console.SetCursorPosition(x_coord, y_coord)
                If i = 0 Or i = UBound(information_text_two) Or i = UBound(information_text_two) - 1 Then
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                Else
                    Console.ForegroundColor = ConsoleColor.Yellow
                End If
                Console.WriteLine(information_text_two(i))
                y_coord += 2
                Console.ResetColor()
            Next

            Console.ForegroundColor = ConsoleColor.Cyan
            Console.SetCursorPosition(75, 35)
            str = "[ ENTER 0 TO RETURN TO PARTS SELECTION ]"
            Console.WriteLine(str)
            Console.ResetColor()

            Console.SetCursorPosition(75, 37)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_asnwer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_asnwer, 0, 1)

            If result Then
                Select Case user_asnwer
                    Case "0"
                        game_information()
                End Select
            Else
                str = "| INVALID VALUE |"
                LineOutPutCharacterByCharacter(str, 75, 37, speed, ConsoleColor.DarkRed)

                Pause(2)
                Console.Clear()
            End If
        Loop

    End Sub
    Sub Game_information_part_one_second_page()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0045

        Dim pic_necessary() As String =
            {
                "╔╗─╔╦═══╦══╦═══╦══╦══╦══╦═══╦╗╔╗",
                "║╚═╝║╔══╣╔═╣╔══╣╔═╣╔═╣╔╗║╔═╗║║║║",
                "║╔╗─║╚══╣║─║╚══╣╚═╣╚═╣╚╝║╚═╝║╚╝║",
                "║║╚╗║╔══╣║─║╔══╩═╗╠═╗║╔╗║╔╗╔╩═╗║",
                "║║─║║╚══╣╚═╣╚══╦═╝╠═╝║║║║║║║─╔╝║",
                "╚╝─╚╩═══╩══╩═══╩══╩══╩╝╚╩╝╚╝─╚═╝"
            }

        Dim pic_information() As String =
            {
                "╔══╦╗─╔╦══╦══╦═══╦╗──╔╦══╦════╦══╦══╦╗─╔╗",
                "╚╗╔╣╚═╝║╔═╣╔╗║╔═╗║║──║║╔╗╠═╗╔═╩╗╔╣╔╗║╚═╝║",
                "─║║║╔╗─║╚═╣║║║╚═╝║╚╗╔╝║╚╝║─║║──║║║║║║╔╗─║",
                "─║║║║╚╗║╔═╣║║║╔╗╔╣╔╗╔╗║╔╗║─║║──║║║║║║║╚╗║",
                "╔╝╚╣║─║║║─║╚╝║║║║║║╚╝║║║║║─║║─╔╝╚╣╚╝║║─║║",
                "╚══╩╝─╚╩╝─╚══╩╝╚╝╚╝──╚╩╝╚╝─╚╝─╚══╩══╩╝─╚╝"
            }

        Dim information_text_one() As String =
            {
                "[ IMPORTANT! THERE ARE NO SAVINGS IN THIS TYPE OF GAMES. ]",
                "THEREFORE, TO KEEP YOUR PROGRESS,",
                "YOU CAN USE SCREENSHOTS OR PHOTOS",
                "TO SHOW IN THE FUTURE TO COLLEAGUES."
            }

        Dim information_text_two() As String =
            {
                "[ ALSO NEEDED TO REMEMBER: ]",
                "- THROUGH THE ENTIRE PROGRAM, THERE ARE TIPS",
                "SUGGESTING WHAT YOU CAN DO AT THE MOMENT.",
                "- IF YOU LOSE PROGRESS DUE TO CERTAIN CIRCUMSTANCES,",
                "YOU CAN RESTORE IT BY OPENING CHAPTERS IN THE GAME SETTINGS",
                "IF POSSIBLE, WE ASK YOU TO WRITE A REVIEW ON YOUR EXPERIENCE",
                "ABOUT THIS GAME.",
                "WHAT WOULD YOU LIKE CHANGE AND IMPROVEMENT?",
                "WE WILL BE VERY PLEASED TO FIND OUT YOUR OPINION.",
                "WRITE TO US ON THE OFFICIAL EMAIL OF OUR STUDIO:",
                "HOPHOP.INC.OFFICIAL@GMAIL.COM"
            }

        Dim stick, str, user_asnwer As String
        Dim x_coord, y_coord As Integer
        stick = "║|"

        Do
            Console.ForegroundColor = ConsoleColor.DarkGray
            For i = 2 To 38
                Console.SetCursorPosition(69, i)
                Console.WriteLine(stick)
            Next
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(pic_necessary, 8, 2, speed - 0.001)
            DisplayingPictureFromSide(pic_information, 79, 2, speed - 0.001)
            Console.ResetColor()

            x_coord = 8
            y_coord = 11
            For i = 0 To UBound(information_text_one)
                Console.SetCursorPosition(x_coord, y_coord)
                If i = 0 Then
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                Else
                    Console.ForegroundColor = ConsoleColor.Yellow
                End If
                Console.WriteLine(information_text_one(i))
                y_coord += 2
                Console.ResetColor()
            Next

            x_coord = 79
            y_coord = 11
            For i = 0 To UBound(information_text_two)
                Console.SetCursorPosition(x_coord, y_coord)
                If i = 0 Then
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                Else
                    Console.ForegroundColor = ConsoleColor.Yellow
                End If
                Console.WriteLine(information_text_two(i))
                y_coord += 2
                Console.ResetColor()
            Next

            Console.ForegroundColor = ConsoleColor.Cyan
            Console.SetCursorPosition(8, 29)
            str = "[ ENTER 0 TO RETURN TO PARTS SELECTION ]"
            Console.WriteLine(str)
            Console.SetCursorPosition(8, 31)
            str = "[ ENTER 1 TO RETURN TO PREVIOUS PAGE ]"
            Console.WriteLine(str)
            Console.ResetColor()

            Console.SetCursorPosition(8, 34)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_asnwer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_asnwer, 0, 1)

            If result Then
                Select Case user_asnwer
                    Case "0"
                        game_information()
                    Case "1"
                        game_information_part_one()
                End Select
            Else
                str = "| INVALID VALUE |"
                LineOutPutCharacterByCharacter(str, 8, 34, speed, ConsoleColor.DarkRed)

                Pause(2)
                Console.Clear()
            End If
        Loop

    End Sub
    Sub Game_information_part_one()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        speed = 0.0045

        Dim pic_basic_info() As String =
            {
                "╔══╗╔══╦══╦══╦══╗╔══╦╗─╔╦══╦══╗",
                "║╔╗║║╔╗║╔═╩╗╔╣╔═╝╚╗╔╣╚═╝║╔═╣╔╗║",
                "║╚╝╚╣╚╝║╚═╗║║║║───║║║╔╗─║╚═╣║║║",
                "║╔═╗║╔╗╠═╗║║║║║───║║║║╚╗║╔═╣║║║",
                "║╚═╝║║║╠═╝╠╝╚╣╚═╗╔╝╚╣║─║║║─║╚╝║",
                "╚═══╩╝╚╩══╩══╩══╝╚══╩╝─╚╩╝─╚══╝"
            }

        Dim pic_directory() As String =
            {
                "╔══╗╔══╦═══╦═══╦══╦════╦══╦═══╦╗╔╗",
                "║╔╗╚╬╗╔╣╔═╗║╔══╣╔═╩═╗╔═╣╔╗║╔═╗║║║║",
                "║║╚╗║║║║╚═╝║╚══╣║───║║─║║║║╚═╝║╚╝║",
                "║║─║║║║║╔╗╔╣╔══╣║───║║─║║║║╔╗╔╩═╗║",
                "║╚═╝╠╝╚╣║║║║╚══╣╚═╗─║║─║╚╝║║║║─╔╝║",
                "╚═══╩══╩╝╚╝╚═══╩══╝─╚╝─╚══╩╝╚╝─╚═╝"
            }

        Dim text_directory() As String =
            {
                "[ THE GREAT WAY BEGINNER ]",
                "[ MAIN MENU ]",
                "- START THE GAME | BEGINNING OF THE GAME |",
                "- SETTINGS | GOING TO GAME SETTINGS (OPENING CHAPTERS + GOLD) |",
                "- INFORMATION | BASIC INFORMATION ABOUT THIS GAME |",
                "- COLLECTION | COLLECTION OF PLAYER'S ITEMS AND POSTERS |",
                "- QUIT GAME | END OF THE PROGRAM SESSION |",
                "[ MAIN MENU / START THE GAME ]",
                "- TAVERN | TRANSITION TO TAVERN |",
                "- DUNGEON | GAME CAMPAIGN START |",
                "- MAIN MENU | RETURN TO THE MAIN MENU OF THE PROGRAM |",
                "[ MAIN MENU / START THE GAME / TAVERN ]",
                "- LARS | INTERACTION WITH LARS (SODA PURCHASE / STAMINA REFILL) |",
                "- INVENTORY | VIEW USER INVENTORY |",
                "- DUNGEON SEASON | VIEWING THE CHALLENGES OF THE GAME SEASON |",
                "- IRON CHEST | PURCHASE | COLLECTION CHEST OPENING |",
                "- PATH MENU | GO TO SELECTION MENU: TAVERN | DUNGEON |",
                "[ MAIN MENU / START THE GAME / DUNGEON ]",
                "- SELECTING A GAME CHAPTER",
                "[ MAIN MENU / START THE GAME / DUNGEON / CHAPTER ]",
                "- VIEWING THE CHAPTER MAP",
                "[ MAIN MENU / START THE GAME / DUNGEON / CHAPTER / LEVEL ]",
                "LEVEL GENERATION MENU | OPPONENT & MODE |"
            }

        Const text_basic_information As String = "Dungeon: The Lost Temple is a console game developed by Hop-Hop.inc. Here you can go through 3 chapters,        receiving a poster for each of them. Fight in a group   of computer opponents! Open chests in the tavern and    get rare items that you can sell to get more gold coins!In the first version of the game, a system of tasks has been added, which are presented in the first game       season! Complete tasks, get posters, show your          colleagues your progress in Dungeon: The Lost Temple!"
        Dim stick, str, user_asnwer As String
        Dim x_coord, y_coord, w As Integer
        stick = "║|"

        Do
            Console.ForegroundColor = ConsoleColor.DarkGray
            For i = 2 To 38
                Console.SetCursorPosition(69, i)
                Console.WriteLine(stick)
            Next
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(pic_basic_info, 8, 2, speed - 0.001)
            DisplayingPictureFromSide(pic_directory, 8, 31, speed - 0.001)
            Console.ResetColor()

            Console.ForegroundColor = ConsoleColor.Yellow
            x_coord = 4
            y_coord = 9
            For i = 0 To text_basic_information.Length - 1
                If i Mod 56 = 0 Then
                    y_coord += 2
                    x_coord = 8
                End If

                Console.SetCursorPosition(x_coord, y_coord)
                Console.Write(text_basic_information(i))

                x_coord += 1
            Next
            Console.ResetColor()

            x_coord = 75
            y_coord = 1
            w = 1
            For i = 0 To UBound(text_directory)
                Console.SetCursorPosition(x_coord, y_coord + w)
                If i = 0 Or i = 1 Or i = 7 Or i = 11 Or i = 17 Or i = 19 Or i = 21 Then
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                    w += 2
                ElseIf i = 6 Or i = 10 Or i = 16 Or i = 18 Or i = 20 Then
                    Console.ForegroundColor = ConsoleColor.Yellow
                    w += 2
                Else
                    Console.ForegroundColor = ConsoleColor.Yellow
                    w += 1
                End If
                Console.WriteLine(text_directory(i))
                Console.ResetColor()
            Next

            Console.SetCursorPosition(109, 38)
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.WriteLine("[ NEXT PAGE - 1 | OUTPUT - 0 ]")
            Console.ResetColor()

            Console.SetCursorPosition(75, 38)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_asnwer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_asnwer, 0, 1)

            If result Then
                Select Case user_asnwer
                    Case "0"
                        game_information()
                    Case "1"
                        Game_information_part_one_second_page()
                End Select
            Else
                str = "| INVALID VALUE |"
                LineOutPutCharacterByCharacter(str, 75, 38, speed, ConsoleColor.DarkRed)

                Pause(2)
                Console.Clear()
            End If
        Loop

    End Sub
    Sub Game_information()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        If Not kingdom_of_snails Then
            speed = 0.0045
        Else
            speed = 0
        End If

        Dim pic_letter_i() As String =
            {
                "██╗",
                "██║",
                "██║",
                "██║",
                "██║",
                "╚═╝"
            }
        Dim pic_letter_n() As String =
            {
                "███╗  ██╗",
                "████╗ ██║",
                "██╔██╗██║",
                "██║╚████║",
                "██║ ╚███║",
                "╚═╝  ╚══╝"
            }
        Dim pic_letter_f() As String =
            {
                "███████╗",
                "██╔════╝",
                "█████╗  ",
                "██╔══╝  ",
                "██║     ",
                "╚═╝     "
            }
        Dim pic_letter_o() As String =
            {
                " █████╗ ",
                "██╔══██╗",
                "██║  ██║",
                "██║  ██║",
                "╚█████╔╝",
                " ╚════╝ "
            }

        Dim pic_part_one() As String =
            {
                "  ██╗  ██████╗  █████╗ ██████╗ ████████╗    ███╗    ██╗  ",
                " ██╔╝  ██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝   ████║    ╚██╗ ",
                "██╔╝   ██████╔╝███████║██████╔╝   ██║     ██╔██║     ╚██╗",
                "╚██╗   ██╔═══╝ ██╔══██║██╔══██╗   ██║     ╚═╝██║     ██╔╝",
                " ╚██╗  ██║     ██║  ██║██║  ██║   ██║     ███████╗  ██╔╝ ",
                "  ╚═╝  ╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝     ╚══════╝  ╚═╝  "
            }
        Dim pic_part_two() As String =
            {
                "  ██╗  ██████╗  █████╗ ██████╗ ████████╗  ██████╗   ██╗  ",
                " ██╔╝  ██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝  ╚════██╗  ╚██╗ ",
                "██╔╝   ██████╔╝███████║██████╔╝   ██║       ███╔═╝   ╚██╗",
                "╚██╗   ██╔═══╝ ██╔══██║██╔══██╗   ██║     ██╔══╝     ██╔╝",
                " ╚██╗  ██║     ██║  ██║██║  ██║   ██║     ███████╗  ██╔╝ ",
                "  ╚═╝  ╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝     ╚══════╝  ╚═╝  "
            }
        Dim pic_part_three() As String =
            {
                "  ██╗  ██████╗  █████╗ ██████╗ ████████╗  ██████╗   ██╗  ",
                " ██╔╝  ██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝  ╚════██╗  ╚██╗ ",
                "██╔╝   ██████╔╝███████║██████╔╝   ██║      █████╔╝   ╚██╗",
                "╚██╗   ██╔═══╝ ██╔══██║██╔══██╗   ██║      ╚═══██╗   ██╔╝",
                " ╚██╗  ██║     ██║  ██║██║  ██║   ██║     ██████╔╝  ██╔╝ ",
                "  ╚═╝  ╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝     ╚═════╝   ╚═╝  "
            }

        Dim info_table_parts() As String =
            {
                "[ BASIC INFORMATION ABOUT THIS GAME ]",
                "[ DIRECTORY ]",
                "[ GAME INFORMATION REQUIRED ]",
                "[ FUTURE DEVELOPMENT OF THIS GAME ]",
                "[ DEVELOPING GAMES LIKE PLAN ]",
                "[ FUTURE VERSIONS OF THIS GAME ]",
                "[ GAME CREATORS ]",
                "[ STUDIO CREATING PROJECT ]",
                "[ CONTACT INFORMATION ]"
            }

        Dim info_table() As String =
            {
                "[ TO RETURN TO THE MAIN MENU ENTER 0 ""ZERO"" ]",
                "[ TO START VIEWING THE INFORMATION, ENTER THE NUMBER OF THE PARTS YOU NEED ]"
            }

        Dim y As Integer
        Dim str, user_answer As String

        Do
            Console.SetCursorPosition(0, 4)
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_letter_i, speed, 130)
            Else
                ImageOutputLineByLine(pic_letter_i, speed + 0.01, 130)
            End If
            Console.SetCursorPosition(0, 11)
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_letter_n, speed, 127)
            Else
                ImageOutputLineByLine(pic_letter_n, speed + 0.01, 127)
            End If
            Console.SetCursorPosition(0, 18)
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_letter_f, speed, 127)
            Else
                ImageOutputLineByLine(pic_letter_f, speed + 0.01, 127)
            End If
            Console.SetCursorPosition(0, 25)
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_letter_o, speed, 127)
            Else
                ImageOutputLineByLine(pic_letter_o, speed + 0.01, 127)
            End If

            Console.SetCursorPosition(0, 4)
            Console.ForegroundColor = ConsoleColor.Blue
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_part_one, speed, 6)
            Else
                ImageOutputLineByLine(pic_part_one, speed + 0.05, 6)
            End If

            Console.ResetColor()
            Console.SetCursorPosition(0, 11)
            Console.ForegroundColor = ConsoleColor.DarkCyan
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_part_two, speed, 6)
            Else
                ImageOutputLineByLine(pic_part_two, speed + 0.05, 6)
            End If

            Console.ResetColor()
            Console.SetCursorPosition(0, 18)
            Console.ForegroundColor = ConsoleColor.Cyan
            If kingdom_of_snails Then
                ImageOutputLineByLine(pic_part_three, speed, 6)
            Else
                ImageOutputLineByLine(pic_part_three, speed + 0.05, 6)
            End If

            Console.ResetColor()

            y = 4
            For i = 0 To UBound(info_table_parts)
                str = info_table_parts(i)
                LineOutPutCharacterByCharacter(str, 66, y, speed, ConsoleColor.DarkYellow)
                If (i + 1) Mod 3 = 0 Then
                    y += 3
                Else
                    y += 2
                End If
            Next

            Console.ForegroundColor = ConsoleColor.Yellow
            Console.SetCursorPosition(6, 27)
            Console.WriteLine(info_table(0))
            Console.SetCursorPosition(6, 29)
            Console.WriteLine(info_table(1))
            Console.ResetColor()

            Console.SetCursorPosition(6, 37)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_answer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_answer, 0, 3)

            If result Then
                Select Case user_answer
                    Case "0"
                        mainMenu()
                    Case "1"
                        Game_information_part_one()
                    Case "2"
                        Game_information_part_two()
                    Case "3"
                        Game_information_part_three()
                End Select
            Else
                str = "[ INVALID VALUE ]"
                LineOutPutCharacterByCharacter(str, 6, 37, speed, ConsoleColor.DarkRed)

                Pause(2)
                Console.Clear()
            End If
        Loop

    End Sub
    'Главное меню
    Sub MainMenu()
        Console.WindowHeight = 40
        Console.WindowWidth = 140
        Console.SetBufferSize(140, 40)
        Console.Clear()

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.0035
        End If



        Dim str1() As String = {
            "╔══╗─╔╗╔╗╔╗─╔╗╔═══╗╔═══╗╔══╗╔╗─╔╗",
            "║╔╗╚╗║║║║║╚═╝║║╔══╝║╔══╝║╔╗║║╚═╝║",
            "║║╚╗║║║║║║╔╗─║║║╔═╗║╚══╗║║║║║╔╗─║",
            "║║─║║║║║║║║╚╗║║║╚╗║║╔══╝║║║║║║╚╗║",
            "║╚═╝║║╚╝║║║─║║║╚═╝║║╚══╗║╚╝║║║─║║",
            "╚═══╝╚══╝╚╝─╚╝╚═══╝╚═══╝╚══╝╚╝─╚╝"
                               }

        Dim str2() As String = {
            "    ║═════════════║                                                                                                        ║═════════════║ ",
            "    ║ ║═════════║ ║                                                                                                        ║ ║═════════║ ║ ",
            "    ║ ╬│║     │║╬ ╬                                                                                                        ║ ╬│║     │║╬ ╬ ",
            "    ╬  │║     │║                                                                                                           ╬  │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "       │║     │║                                                                                                              │║     │║    ",
            "      ║═════════║                                                                                                            ║═════════║   ",
            "    ║═════════════║                                                                                                        ║═════════════║ "
                               }

        Dim str3() As String = {
            "═════════════════════════════════════════════════════",
            "█████████████████████████████████████████████████████",
            "█████████████████████████████████████████████████████",
            "█████████████████████████████████████████████████████",
            "█████████████████████████████████████████████████████",
            "═════════════════════════════════════════════════════"
                               }

        Dim str4() As String = {
            "═════════════════════════════════════════",
            "                                     ══  ",
            "                                     ══  ",
            "                                     ══  ",
            "                                     ══  ",
            "                                     ══  ",
            "                                     ══  ",
            "   ══════════════════════════════════════"
                               }

        Dim str4Rev() As String = {
            "═══════════════════════════════════════════════",
            "  ══                                           ",
            "  ══                                           ",
            "  ══                                           ",
            "  ══                                           ",
            "  ══                                           ",
            "  ══                                           ",
            "════════════════════════════════════════════   "
                                  }

        Dim str5() As String = {
            "    ══╬══╬══    ",
            "  ╬   ║╬╬║   ╬  ",
            "  ║║══║║║║══║║  ",
            "  ║║   ║║   ║║  ",
            " ╬╬║   ║║   ║╬╬ ",
            " ║ ║ │ ║║ │ ║ ║ ",
            "╬╬ ║ │ ║║ │ ║ ╬╬",
            "║  ║ ┼ ║║ ┼ ║  ║",
            "╬  ║   ║║   ║  ╬"
                               }

        Dim str8() As String = {
            "╔╗╔╗╔╗ ╔═══╗ ╔══╗ ╔╗─╔╗ ╔═══╗",
            "║║║║║║ ║╔═╗║ ║╔╗║ ║╚═╝║ ║╔══╝",
            "║║║║║║ ║╚═╝║ ║║║║ ║╔╗─║ ║║╔═╗",
            "║║║║║║ ║╔╗╔╝ ║║║║ ║║╚╗║ ║║╚╗║",
            "║╚╝╚╝║ ║║║║─ ║╚╝║ ║║─║║ ║╚═╝║",
            "╚═╝╚═╝ ╚╝╚╝─ ╚══╝ ╚╝─╚╝ ╚═══╝"
                               }

        Dim str9() As String = {
            "╔══╗ ╔═══╗ ╔═══╗ ╔╗── ╔╗  ",
            "║╔═╝ ║╔═╗║ ║╔══╝ ║║── ║║  ",
            "║╚═╗ ║╚═╝║ ║╚══╗ ║║── ║║  ",
            "╚═╗║ ║╔══╝ ║╔══╝ ║║── ║║  ",
            "╔═╝║ ║║─── ║╚══╗ ║╚═╗ ║╚═╗",
            "╚══╝ ╚╝─── ╚═══╝ ╚══╝ ╚══╝"
                                }

        Dim menuItems() As String = {
            "START THE GAME",
            "SETTINGS",
            "INFORMATION",
            "COLLECTION",
            "QUIT GAME"
                                    }

        Dim l As Integer
        Dim userAns, str As String
        Dim pass As Boolean
        Dim symb As Char

        Do
            'Выводим оформление окна
            '|Dungeon|
            Console.ForegroundColor = ConsoleColor.Magenta
            Console.Write(Chr(10) & Chr(10))
            If kingdom_of_snails Then
                ImageOutputLineByLine(str1, 0, 53)
            Else
                ImageOutputLineByLine(str1, 0.07, 53)
            End If
            Console.ResetColor()
            Console.Write(Chr(10) & Chr(10))
            'Башни
            Console.ForegroundColor = ConsoleColor.DarkCyan
            If kingdom_of_snails Then
                ImageOutputLineByLine(str2, 0, 0)
            Else
                ImageOutputLineByLine(str2, 0.04, 0)
            End If

            Console.ResetColor()
            'Линии из |Dungeon|
            Console.ForegroundColor = ConsoleColor.Cyan
            'Линия (1)
            If kingdom_of_snails Then
                DisplayingPictureFromSide(str3, 0, 2, speed)
                'Линия (2)
                DisplayingPictureFromSide(str3, 86, 2, speed)
                Console.ResetColor()
            Else
                DisplayingPictureFromSide(str3, 0, 2, speed - 0.0001)
                Pause(0.1)
                'Линия (2)
                DisplayingPictureFromSide(str3, 86, 2, speed - 0.0001)
                Console.ResetColor()
            End If

            'Выводим стену между башнями #1
            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(str4, 19, 31, speed - 0.0001)
            Console.ResetColor()

            'Выводим ворота
            Console.ForegroundColor = ConsoleColor.DarkCyan
            DisplayingPictureFromSide(str5, 60, 30, speed - 0.0001)
            Console.ResetColor()

            'Выводим стену между башнями #2
            Console.ForegroundColor = ConsoleColor.Cyan
            DisplayingPictureFromSide(str4Rev, 76, 31, speed - 0.0001)
            Console.ResetColor()

            'Выводим пункты меню
            Console.SetCursorPosition(60, 12)
            l = 12
            For i = 0 To UBound(menuItems)
                Select Case i
                    Case 1 To 3
                        Console.SetCursorPosition(60, l)
                    Case 4
                        Console.SetCursorPosition(60, l + 1)
                End Select
                If i <> 4 Then
                    Console.ForegroundColor = ConsoleColor.Cyan
                    Console.Write(i + 1 & "|" & " ")
                    Console.ResetColor()
                Else
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                    Console.Write("0" & "|" & " ")
                    Console.ResetColor()
                End If
                str = menuItems(i)
                If i <> 4 Then
                    For j = 0 To menuItems(i).Length - 1
                        symb = str(j)
                        Console.Write(symb)
                        If Not kingdom_of_snails Then
                            Pause(0.04)
                        End If
                    Next
                Else
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                    For j = 0 To menuItems(i).Length - 1
                        symb = str(j)
                        Console.Write(symb)
                        If Not kingdom_of_snails Then
                            Pause(0.04)
                        End If
                    Next
                    Console.ResetColor()
                End If
                l += 2

                If Not kingdom_of_snails Then
                    Pause(0.3)
                Else
                    Pause(0.01)
                End If
            Next

            'Выводим строку ввода пользователя
            Console.SetCursorPosition(56, l + 2)
            Console.ForegroundColor = ConsoleColor.Magenta

            str = "| CHOOSE THE MENU ITEM |"
            For i = 0 To str.Length - 1
                symb = str(i)
                Console.Write(symb)
                If Not kingdom_of_snails Then
                    Pause(0.03)
                End If
            Next

            Console.ResetColor()
            Console.SetCursorPosition(58, 28)
            str = "════════════════════"
            For i = 0 To str.Length - 1
                symb = str(i)
                Console.Write(symb)
                If Not kingdom_of_snails Then
                    Pause(0.04)
                End If
            Next
            Console.SetCursorPosition(58, l + 4)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            userAns = Console.ReadLine()
            Console.ResetColor()
            result = CheckUserAns(userAns, 0, 4)

            If result Then
                pass = True
            Else
                'Выводим |Wrong|
                Console.ForegroundColor = ConsoleColor.DarkRed
                If kingdom_of_snails Then
                    DisplayingPictureFromSide(str8, 23, 32, 0)
                Else
                    DisplayingPictureFromSide(str8, 23, 32, 0.0001)
                End If

                Pause(0.4)

                'Выводим |Spell|
                Console.ForegroundColor = ConsoleColor.DarkRed
                If kingdom_of_snails Then
                    DisplayingPictureFromSide(str9, 88, 32, 0)
                Else
                    DisplayingPictureFromSide(str9, 88, 32, 0.0001)
                End If
                Pause(1)

                Console.ResetColor()
            End If
            Console.Clear()
        Loop Until pass

        Select Case userAns
            Case 0
                'Переход в окно выхода программы
                ExitWindow()
            Case 1
                'Переход в начало игры | Выбор: Таверна / Подземелье
                PathMenu()
            Case 2
                'Переход в настройки игры
                Game_settings_first_page()
            Case 3
                'Переход в информационную
                Game_information()
            Case 4
                'Переход в колелкцию
                User_collection()
        End Select

    End Sub
    'Соглашение пользователя
    Sub User_verification()
        Console.WindowHeight = 30
        Console.WindowWidth = 120
        Console.SetBufferSize(120, 30)

        If kingdom_of_snails Then
            speed = 0
        Else
            speed = 0.01
        End If

        Console.Clear()

        Dim text_info() As String =
            {
                "| THIS GAME IS NOT PERFECT, IT IS CREATED BY THE INITIATIVE OF A YOUNG DEVELOPER |",
                "| YOU CAN DISABLE ALL ANIMATIONS IN THE GAME SETTINGS |",
                "| CHANGING THE CONSOLE SIZE AND PRESSING THE SCREEN WITH THE MOUSE MAY CAUSE",
                "  THE PROGRAM NOT WORKING CORRECTLY |",
                "| LONG DOWNLOAD OF OUTPUT ANIMATIONS IS POSSIBLE WHEN YOUR PROCESSOR IS OPERATING",
                "  FOR LONG TIME |"
            }
        Dim str, user_answer As String
        Dim x_coord, y_coord As Integer

        Do
            x_coord = 19
            y_coord = 7
            For i = 0 To UBound(text_info)
                str = text_info(i)
                LineOutPutCharacterByCharacter(str, x_coord, y_coord, speed, ConsoleColor.Yellow)
                y_coord += 2
            Next

            Console.ForegroundColor = ConsoleColor.Cyan
            Console.SetCursorPosition(x_coord, y_coord + 4)
            str = "1. [ CONFIRM | PROCEED ]"
            Console.WriteLine(str)

            Console.SetCursorPosition(x_coord + 30, y_coord + 4)
            str = "2. [ DECLINE | LEAVE THE GAME ] "
            Console.WriteLine(str)
            Console.ResetColor()

            Console.SetCursorPosition(x_coord, y_coord + 6)
            Console.Write("-> ")
            Console.ForegroundColor = ConsoleColor.Cyan
            user_answer = Console.ReadLine()
            Console.ResetColor()

            result = CheckUserAns(user_answer, 1, 2)

            If result Then
                Select Case user_answer
                    Case "1"
                        k += 1
                        Main()
                    Case "2"
                        End
                End Select
            Else
                Console.Clear()
                speed = 0
            End If
        Loop
    End Sub
    'Вывод второго окна загрузки |Dungeon: Lost Temple|
    Sub ShowSecondOutPost()
        Console.WindowHeight = 40
        Console.WindowWidth = 122
        Console.SetBufferSize(122, 40)

        Dim l As Integer

        Dim str1() As String = {
            "╔══╗═════╗",
            "║╔╗║══╗╔╗║",
            "║╚═╝╔╗║╚═╝",
            "╚══║╚═╝══╗",
            "╔═╗║╔╗╔═╗║",
            "║╚╝╚═╝║╚╝║",
            "╚═════╚══╝"
                               }

        Dim str2() As String = {
            "           .░▒▒▒▒▒▒░. ",
            "          ▒▒▒░▒░░░░▒▒.",
            "          ░▒▒▒░▒▒▒░░▒▒",
            "    _    ░░░▒▒░░░▒░▒▒ ",
            "    ▓_  ░░░░░░`▒▒▒▒▒  ",
            "    ▓▓ ░░░░░░▒░░░░░░  ",
            "  ▓_ ▓▓░░░░░▒░░░░░░░  ",
            " ▓▓▓ ▓░░░░░▒░░░░░░░ ▓ ",
            "_ ▓▓▓▓░░░░▒░░░░░░░▓▓  ",
            "▓_ ▓▓▓░▓░▒░░░░░░▓▓    ",
            "▓▓__'▓▓▓░▒░░░░▓▓▓     ",
            "▓ ▓▓__▓▓'▓▓'▓▓▓▓      ",
            "▓▓ ▓▓▓__  ▓▓▓         ",
            "▓▓ ▓▓▓▓▓  ▓▓          ",
            "▓▓▓ ▓▓▓▓  ▓▓          ",
            " ▓▓▓ ▓▓▓_ ▓▓          ",
            "  ▓▓▓ ▓▓▓ ▓▓          ",
            "     ▓▓▓_ ▓▓          ",
            "       ▓▓ ▓▓          "
                               }

        Dim str3() As String = {
            "██████╗░██╗░░░██╗███╗░░██╗░██████╗░███████╗░█████╗░███╗░░██╗",
            "██╔══██╗██║░░░██║████╗░██║██╔════╝░██╔════╝██╔══██╗████╗░██║",
            "██║░░██║██║░░░██║██╔██╗██║██║░░██╗░█████╗░░██║░░██║██╔██╗██║",
            "██║░░██║██║░░░██║██║╚████║██║░░╚██╗██╔══╝░░██║░░██║██║╚████║",
            "██████╔╝╚██████╔╝██║░╚███║╚██████╔╝███████╗╚█████╔╝██║░╚███║",
            "╚═════╝░░╚═════╝░╚═╝░░╚══╝░╚═════╝░╚══════╝░╚════╝░╚═╝░░╚══╝"
                               }

        Dim str4() As String = {
            "            ▀▀█▀▀ █░░█ █▀▀                        ",
            "              █░░ █▀▀█ █▀▀                        ",
            "              █░░ ▀░░▀ ▀▀▀                        ",
            "                                                  ",
            "█░░ █▀▀█ █▀▀ ▀▀╬▀▀═║║═▀▀╬▀▀ █▀▀ █▀▄▀█ █▀▀█ █░░ █▀▀",
            "█░░ █░░█ ▀▀█ ░░║░░ ║║ ░░║░░ █▀▀ █░▀░█ █░░█ █░░ █▀▀",
            "▀▀▀ ▀▀▀▀ ▀▀▀ ░░╬░░ ║║ ░░╬░░ ▀▀▀ ▀░░░▀ █▀▀▀ ▀▀▀ ▀▀▀"
                               }

        Dim str5() As String = {
            "══╗",
            "╔╗║",
            "╚═╝"
                               }

        Dim str6() As String = {
            "╔═════════════════════════════║-║══════════════════════════════╗",
            "║╗░░░░║░░░░║░░░░║░░░░║░░░░║░░╔║║║╗░░║░░░░║░░░░║░░░░║░░░░║░░░░░╔║",
            "╚╝████║████║████║████║████║██╚║║║╝██║████║████║████║████║█████╚╝"
                               }

        Dim str7() As String = {
            "║",
            "║"
                               }

        Console.ForegroundColor = ConsoleColor.White
        DisplayingPictureFromSide(str1, 3, 0, 0.0019)
        Console.ResetColor()

        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(str1, 109, 0, 0.0019)
        Console.ResetColor()

        Console.ForegroundColor = ConsoleColor.White
        DisplayingPictureFromSide(str1, 3, 33, 0.0019)
        Console.ResetColor()

        Console.ForegroundColor = ConsoleColor.Magenta
        'Вывод линии соединения цветков |Верхний левый угол - Нижний левый угол|
        Console.SetCursorPosition(6, 7)
        l = 6
        For i = 1 To 27
            Console.Write("║║║║")
            l += 1
            Console.SetCursorPosition(6, l)
            Pause(0.015)
        Next
        Console.ForegroundColor = ConsoleColor.Magenta
        'Вывод линии соединения цветков |Верхний левый угол - Верхний правый угол|
        Console.SetCursorPosition(10, 2)
        l = 10
        For i = 1 To 32
            l += 3
            DisplayingPictureFromSide(str5, l, 2, 0.0037)
            Pause(0.005)
        Next
        'Вывод розы
        DisplayingPictureFromSide(str2, 95, 20, 0.0038)

        'Вывод |Dungeon|
        Console.ForegroundColor = ConsoleColor.DarkYellow
        DisplayingPictureFromSide(str3, 18, 12, 0.0037)
        Console.ResetColor()

        'Вывод |The Lost Temple|
        Console.ForegroundColor = ConsoleColor.DarkMagenta
        DisplayingPictureFromSide(str4, 18, 21, 0.0038)
        Console.ResetColor()

        Console.ForegroundColor = ConsoleColor.DarkGray
        'Вывод линии |Между Dungeon и Lost Temple|
        Console.SetCursorPosition(18, 19)
        For i = 1 To 60
            Console.Write("═")
            Pause(0.01)
        Next
        Console.ResetColor()

        'Вывод стенки под |Lost Temple|
        Console.ForegroundColor = ConsoleColor.DarkGray
        Console.SetCursorPosition(18, 36)
        l = 36
        For i = 0 To UBound(str6)
            Console.Write(str6(i))
            l += 1
            Console.SetCursorPosition(18, l)
            Pause(0.08)
        Next
        Console.ResetColor()

        'Загрузка завершения показа второго окна
        Console.SetCursorPosition(24, 37)
        l = 24
        For i = 0 To 10
            Console.ForegroundColor = ConsoleColor.Cyan
            For j = 0 To UBound(str7)
                Console.WriteLine(str7(j))
                Console.SetCursorPosition(l, 38)
            Next
            Console.ResetColor()
            l += 5
            Console.SetCursorPosition(l, 37)
            Pause(0.55)
        Next

        k += 1
        Main()
    End Sub
    'Вывод первого окна загрузки |Hop-hop.ind present|
    Sub ShowFirstOutPost()
        Console.WindowHeight = 30
        Console.WindowWidth = 120
        Console.SetBufferSize(120, 30)

        Dim str1() As String = {
            "██╗░░██╗░█████╗░██████╗░░░░░░░██╗░░██╗░█████╗░██████╗░░░░██╗███╗░░██╗░█████╗░",
            "██║░░██║██╔══██╗██╔══██╗░░░░░░██║░░██║██╔══██╗██╔══██╗░░░██║████╗░██║██╔══██╗",
            "███████║██║░░██║██████╔╝█████╗███████║██║░░██║██████╔╝░░░██║██╔██╗██║██║░░╚═╝",
            "██╔══██║██║░░██║██╔═══╝░╚════╝██╔══██║██║░░██║██╔═══╝░░░░██║██║╚████║██║░░██╗",
            "██║░░██║╚█████╔╝██║░░░░░░░░░░░██║░░██║╚█████╔╝██║░░░░░██╗██║██║░╚███║╚█████╔╝",
            "╚═╝░░╚═╝░╚════╝░╚═╝░░░░░░░░░░░╚═╝░░╚═╝░╚════╝░╚═╝░░░░░╚═╝╚═╝╚═╝░░╚══╝░╚════╝░"
                               }

        Dim str2() As String = {
            "██████╗░██████╗░███████╗░██████╗███████╗███╗░░██╗████████╗",
            "██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝████╗░██║╚══██╔══╝",
            "██████╔╝██████╔╝█████╗░░╚█████╗░█████╗░░██╔██╗██║░░░██║░░░",
            "██╔═══╝░██╔══██╗██╔══╝░░░╚═══██╗██╔══╝░░██║╚████║░░░██║░░░",
            "██║░░░░░██║░░██║███████╗██████╔╝███████╗██║░╚███║░░░██║░░░",
            "╚═╝░░░░░╚═╝░░╚═╝╚══════╝╚═════╝░╚══════╝╚═╝░░╚══╝░░░╚═╝░░░"
                                }

        Console.WriteLine(Chr(10) & Chr(10) & Chr(10))

        Console.ForegroundColor = ConsoleColor.Magenta
        ImageOutputLineByLine(str1, 0.1, 19)
        Console.ResetColor()

        Console.WriteLine(Chr(10) & Chr(10) & Chr(10))

        Console.ForegroundColor = ConsoleColor.White
        DisplayingPictureFromSide(str2, 19, 12, 0.008)
        Console.ResetColor()

        Overload(50, 25)

        k += 1
        Main()
    End Sub
    Function IsFile(ByVal fName As String) As Boolean
        'Returns TRUE if the provided name points to an existing file.
        'Returns FALSE if not existing, or if it a folder
        On Error Resume Next
        IsFile = ((GetAttr(fName) And vbDirectory) <> vbDirectory)
    End Function
    'Основная процедура
    Sub Main()
        Console.Title = "Dungeon: The Lost Temple"
        Console.Clear()
        Console.WindowHeight = 40
        Console.WindowWidth = 160

        Dim l As Integer
        Dim str As String
        str = ""

        If Not IsFile("radio_room/save.txt") Then
            Dim path As String = "radio_room/save.txt"
            Dim fs As FileStream = File.Create(path)
            fs.Close()

            Check_tasks_for_complete()
        End If

        FileOpen(1, "radio_room/save.txt", OpenMode.Input)
        Do While Not EOF(1)
            str = LineInput(1) 'ввод строки из файла с номером 1

            Select Case l
                Case 0
                    'playerCoins
                    Dim new_str() As String = Split(str, "= ")
                    playerCoins = new_str(1)
                Case 1
                    'inventory
                    Dim inventory_str() As String = Split(str, "= ")
                    Dim inventory_items() As String = Split(inventory_str(1), ",")

                    For i = 0 To UBound(playerInventoryNames)
                        playerInventory(i) = True
                        playerInventoryNames(i) = inventory_items(i)
                        If playerInventoryNames(i) <> "NULL" Then
                            playerBag += 1
                        End If
                    Next
                Case 2
                    Dim done_chapters_str() As String = Split(str, "= ")
                    Dim chapters_str() As String = Split(done_chapters_str(1), ",")

                    For i = 0 To UBound(chapters_str)
                        Select Case i
                            Case 0
                                If chapters_str(0) = "1" Then
                                    doneChapters(0) = True

                                    For j = 0 To UBound(doneLevelsFromCharpet1)
                                        doneLevelsFromCharpet1(j) = True
                                    Next
                                End If
                            Case 1
                                If chapters_str(1) = "2" Then
                                    doneChapters(1) = True

                                    For j = 0 To UBound(doneLevelsFromCharpet2)
                                        doneLevelsFromCharpet2(j) = True
                                    Next
                                End If
                            Case 2
                                If chapters_str(2) = "3" Then
                                    doneChapters(2) = True

                                    For j = 0 To UBound(doneLevelsFromCharpet3)
                                        doneLevelsFromCharpet3(j) = True
                                    Next
                                End If
                        End Select
                    Next
                Case 3
                    Dim done_levels_from_chapter1_str() As String = Split(str, "= ")
                    Dim chapter1_str() As String = Split(done_levels_from_chapter1_str(1), ",")

                    For i = 0 To UBound(chapter1_str)
                        Select Case i
                            Case 0
                                If chapter1_str(0) = "1" Then
                                    doneLevelsFromCharpet1(0) = True
                                Else
                                    doneLevelsFromCharpet1(0) = False
                                End If
                            Case 1
                                If chapter1_str(1) = "2" Then
                                    doneLevelsFromCharpet1(1) = True
                                Else
                                    doneLevelsFromCharpet1(1) = False
                                End If
                            Case 2
                                If chapter1_str(2) = "3" Then
                                    doneLevelsFromCharpet1(2) = True
                                Else
                                    doneLevelsFromCharpet1(2) = False
                                End If
                            Case 3
                                If chapter1_str(3) = "4" Then
                                    doneLevelsFromCharpet1(3) = True
                                Else
                                    doneLevelsFromCharpet1(3) = False
                                End If
                        End Select
                    Next
                Case 4
                    Dim done_levels_from_chapter2_str() As String = Split(str, "= ")
                    Dim chapter2_str() As String = Split(done_levels_from_chapter2_str(1), ",")

                    For i = 0 To UBound(chapter2_str)
                        Select Case i
                            Case 0
                                If chapter2_str(0) = "1" Then
                                    doneLevelsFromCharpet2(0) = True
                                Else
                                    doneLevelsFromCharpet2(0) = False
                                End If
                            Case 1
                                If chapter2_str(1) = "2" Then
                                    doneLevelsFromCharpet2(1) = True
                                Else
                                    doneLevelsFromCharpet2(1) = False
                                End If
                            Case 2
                                If chapter2_str(2) = "3" Then
                                    doneLevelsFromCharpet2(2) = True
                                Else
                                    doneLevelsFromCharpet2(2) = False
                                End If
                            Case 3
                                If chapter2_str(3) = "4" Then
                                    doneLevelsFromCharpet2(3) = True
                                Else
                                    doneLevelsFromCharpet2(3) = False
                                End If
                        End Select
                    Next
                Case 5
                    Dim done_levels_from_chapter3_str() As String = Split(str, "= ")
                    Dim chapter3_str() As String = Split(done_levels_from_chapter3_str(1), ",")

                    For i = 0 To UBound(chapter3_str)
                        Select Case i
                            Case 0
                                If chapter3_str(0) = "1" Then
                                    doneLevelsFromCharpet3(0) = True
                                Else
                                    doneLevelsFromCharpet3(0) = False
                                End If
                            Case 1
                                If chapter3_str(1) = "2" Then
                                    doneLevelsFromCharpet3(1) = True
                                Else
                                    doneLevelsFromCharpet3(1) = False
                                End If
                            Case 2
                                If chapter3_str(2) = "3" Then
                                    doneLevelsFromCharpet3(2) = True
                                Else
                                    doneLevelsFromCharpet3(2) = False
                                End If
                            Case 3
                                If chapter3_str(3) = "4" Then
                                    doneLevelsFromCharpet3(3) = True
                                Else
                                    doneLevelsFromCharpet3(3) = False
                                End If
                        End Select
                    Next
                Case 6
                    Dim done_tasks_str() As String = Split(str, "= ")
                    Dim tasks_str() As String = Split(done_tasks_str(1), ",")

                    For i = 0 To UBound(tasks_str)

                        Select Case i
                            Case 0
                                If tasks_str(i) = "1" Then
                                    done_first_season_tasks(0) = True
                                Else
                                    done_first_season_tasks(0) = False
                                End If
                            Case 1
                                If tasks_str(i) = "2" Then
                                    done_first_season_tasks(1) = True
                                Else
                                    done_first_season_tasks(1) = False
                                End If
                            Case 2
                                If tasks_str(i) = "3" Then
                                    done_first_season_tasks(2) = True
                                Else
                                    done_first_season_tasks(2) = False
                                End If
                            Case 3
                                If tasks_str(i) = "4" Then
                                    done_first_season_tasks(3) = True
                                Else
                                    done_first_season_tasks(3) = False
                                End If
                            Case 4
                                If tasks_str(i) = "5" Then
                                    done_first_season_tasks(4) = True
                                Else
                                    done_first_season_tasks(4) = False
                                End If
                            Case 5
                                If tasks_str(i) = "6" Then
                                    done_first_season_tasks(5) = True
                                Else
                                    done_first_season_tasks(5) = False
                                End If
                            Case 6
                                If tasks_str(i) = "7" Then
                                    done_first_season_tasks(6) = True
                                Else
                                    done_first_season_tasks(6) = False
                                End If
                            Case 7
                                If tasks_str(i) = "8" Then
                                    done_first_season_tasks(7) = True
                                Else
                                    done_first_season_tasks(7) = False
                                End If
                            Case 8
                                If tasks_str(i) = "9" Then
                                    done_first_season_tasks(8) = True
                                Else
                                    done_first_season_tasks(8) = False
                                End If
                            Case 9
                                If tasks_str(i) = "10" Then
                                    done_first_season_tasks(9) = True
                                Else
                                    done_first_season_tasks(9) = False
                                End If
                            Case 10
                                If tasks_str(i) = "11" Then
                                    done_first_season_tasks(10) = True
                                Else
                                    done_first_season_tasks(10) = False
                                End If
                            Case 11
                                If tasks_str(i) = "12" Then
                                    done_first_season_tasks(11) = True
                                Else
                                    done_first_season_tasks(11) = False
                                End If
                        End Select
                    Next
                Case 7
                    Dim done_tasks_str() As String = Split(str, "= ")
                    Dim tasks_str() As String = Split(done_tasks_str(1), ",")

                    For j = 0 To UBound(first_time_done_tasks)
                        If tasks_str(j) = "0" Then
                            first_time_done_tasks(j) = False
                        ElseIf tasks_str(j) = "1" Then
                            first_time_done_tasks(j) = True
                        End If
                    Next
                Case 8
                    Dim done_tasks_str() As String = Split(str, "= ")

                    If done_tasks_str(1) = "True" Then
                        kingdom_of_snails = True
                    Else
                        kingdom_of_snails = False
                    End If
            End Select

            l += 1
        Loop
        FileClose(1)

        Select Case k
            Case 0
                ShowFirstOutPost()
            Case 1
                user_verification()
            Case 2
                ShowSecondOutPost()
            Case 3
                MainMenu()
        End Select

        Console.ReadKey()
    End Sub
End Module