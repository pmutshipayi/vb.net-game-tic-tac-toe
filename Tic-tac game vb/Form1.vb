Public Class Form1
    Public playersName(2)
    Dim player1Move As String = ""
    Dim player2Move As String = ""
    Dim playersSign() As String = {"X", "O"}
    Public player1Score As Integer
    Public player2Score As Integer
    Dim CurrentPlayer = 0
    Dim Counter = 0
    Dim _tempCombinaison() As String
    Public PlayWithCPU As Boolean = False ' set to false later
    Public isGameRuning As Boolean = False  ' set to false later
    Dim s As Boolean = False

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.Text = "Tic tac game by parfait"
        playersName(0) = "Player 1"
        playersName(1) = "CPU"
        GroupBox1.Hide()
        ' test
        Dim a() As String = {1, 4, 2, 9}
        Dim b() As String = {8, 6, 9, 4, 3}
        ' Dim t As Boolean = Checkers(a, "get-matcher")
        ' MsgBox(t)
        'If t Then
        ' MsgBox(getMissMatch(a, _tempCombinaison))
        ' End If
    End Sub
    '      Butttons
    '
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click, Button2.Click, Button3.Click, Button4.Click, Button5.Click, Button6.Click, Button8.Click, Button9.Click, Button7.Click
        ' get the button clicked
        Dim btn As Button = sender
        btn.Font = New Font("comic sans MS", 20, FontStyle.Bold)
        If CurrentPlayer = 0 Then
            btn.ForeColor = Color.Green

        Else
            btn.ForeColor = Color.Blue
        End If
        ' check if the game is running
        If isGameRuning Then
            ' is the turn of the CPU to play 
            If isTurnOfCPU() Then
                MsgBox("it's not your turn to play")
            Else
                ' check if the button it is already used or not
                If btn.Text = Nothing Then
                    ' ok the button clicked is not used
                    ' record the key 
                    Dim foundWinner As Boolean = False
                    recordMovement(btn.Name.Replace("Button", ""))
                    ' try to validate
                    If Counter >= 4 Then
                        If Checkers(gett("move").Split(","), "") Then
                            MsgBox(gett("name") & " Won !")
                            plus1toTheWinner()
                            clear()
                            foundWinner = True
                            s = True
                        End If
                    End If
                    If Counter >= 8 Then
                        MsgBox("No one won")
                        clear()
                        foundWinner = True
                    End If
                    If foundWinner = False Then
                        btn.Text = playersSign(CurrentPlayer)
                        changePlayer()
                        clearMsg()
                        Counter = Counter + 1
                    End If
                Else
                    ' the button cliked is already used 
                    sendMsg("Invalid move !", 0)
                End If
            End If
        Else
            sendMsg("the game is not yet started, please go on menu and create one!", 0)
        End If
    End Sub
    Sub plus1toTheWinner()
        If CurrentPlayer = 0 Then
            player1Score = player1Score + 1
        Else
            player2Score = player2Score + 1
        End If
        ' update the score!
        writePlayers()
    End Sub
    Function gett(ByVal args As String) As String
        Select Case args
            Case "move"
                If CurrentPlayer = 0 Then
                    Return player1Move
                Else
                    Return player2Move
                End If
            Case "name"
                Return playersName(CurrentPlayer)
        End Select
        Return Nothing
    End Function
    Sub recordMovement(ByVal key As String)
        If CurrentPlayer = 0 Then
            player1Move += key & ","
        Else
            player2Move += key & ","
        End If
    End Sub
    Sub changePlayer()
        If CurrentPlayer = 0 Then
            CurrentPlayer = 1
        Else
            CurrentPlayer = 0
        End If
        If isTurnOfCPU() Then
            CPU_PLAY()
        End If
    End Sub
    Function isTurnOfCPU() As Boolean
        If PlayWithCPU = True Then
            If gett("name") = "CPU" Then
                Return True
            End If
        End If
        Return False
    End Function
    Sub CPU_PLAY()
        ' get list of all unused buttons
        Dim listVaibleBtn() = getAllUnusedBtn()
        Dim btn As Button
        If Checkers(player2Move.Split(","), "get-matcher") Then
            ' try to win
            Dim str As String = getMissMatch(player2Move.Split(","), _tempCombinaison)
            btn = DirectCast(Controls("button" & str), Button)
            If s Then
                CPU_PLAY_RANDOM()
            ElseIf btn.Text = "" Then
                btn.Text = playersSign(CurrentPlayer)
                ' player2Move += str & ","
                recordMovement(str)
                If Checkers(player2Move.Split(","), "") Then
                    MsgBox("CPU won")
                    plus1toTheWinner()
                    clear()
                End If
                changePlayer()
                Counter = Counter + 1
            Else
                CPU_PLAY_RANDOM()
                MsgBox("Gen on win")
            End If
            ' MsgBox("Detect wi possibility on " & str)

        ElseIf Checkers(player1Move.Split(","), "get-matcher") Then
            ' deffending
            Dim str As String = getMissMatch(player1Move.Split(","), _tempCombinaison)
            btn = DirectCast(Controls("button" & str), Button)
            If btn.Text = "" Then
                btn.Text = playersSign(CurrentPlayer)
                ' player2Move += str & ","
                recordMovement(str)
                changePlayer()
                Counter = Counter + 1
            Else
                CPU_PLAY_RANDOM()
            End If
        Else
            ' just generate
            'MsgBox("Just generate")
            CPU_PLAY_RANDOM()
        End If
        'changePlayer()
    End Sub
    Sub CPU_PLAY_RANDOM()
        Dim btn As Button
        Dim avaibleBtn() = getAllUnusedBtn()
        Dim errorPlaced = True
        Dim genereted As String = ""
        While errorPlaced
            Dim genVal As Integer = CInt(Int((avaibleBtn.Length - 1) * Rnd()) + 0)
            btn = DirectCast(Controls("button" & avaibleBtn(genVal)), Button)
            If btn.Text = "" Then
                btn.Text = playersSign(CurrentPlayer)
                errorPlaced = False
                genereted = btn.Name.Replace("Button", "")
            End If
        End While
        ' player2Move += genVal & ","
        recordMovement(genereted)
        changePlayer()
        Counter = Counter + 1
    End Sub
    Function getAllUnusedBtn() As String()
        Dim list As String = ""
        For i = 0 To 9
            Dim btn As Button = DirectCast(Controls("button" & i), Button)
            Try
                If btn.Text = "" Then
                    list += i & ","
                End If
            Catch ex As Exception

            End Try
        Next
        Return list.Split(",")
    End Function
    Function findInArray(ByRef arr As String(), ByVal val As String) As Boolean
        Dim f As Boolean = False
        For Each x As String In arr
            If x = val Then
                f = True
            End If
        Next
        Return f
    End Function
    Function removeInArray(ByVal arr As String(), ByVal elem As String) As String()
        Dim result As String = ""
        For i = 0 To arr.Length - 1
            If arr(i) <> elem Then
                result += arr(i) & ","
            End If
        Next
        Return result.Split(",")
    End Function
    Function compareArray(ByVal arr1 As String(), ByVal arr2 As String(), ByVal args As String) As Boolean
        Dim counter As Integer = 0
        For Each elem As Integer In arr1
            If findInArray(arr2, elem) Then
                arr2 = removeInArray(arr2, elem)
                counter = counter + 1
            End If
        Next
        Select Case args.ToLower
            Case "get-matcher"
                If counter = 2 Then
                    Return True
                End If
            Case Else
                If counter = 3 Then
                    Return True
                Else
                    Return False
                End If
        End Select
        Return False
    End Function
    Function getMissMatch(ByVal arr1 As String(), ByVal arr2 As String()) As String
        ' we assume that the miss match will return only one element
        ' find the biggest array
        Dim b As Integer = 0
        Dim f As String = ""
        If arr1.Length > arr2.Length Then
            b = arr1.Length
        Else
            b = arr2.Length
        End If
        For i = 0 To b - 1
            Try
                If findInArray(arr1, arr2(i)) = False Then
                    f = arr2(i)
                End If
            Catch ex As Exception

            End Try
        Next
        Return f
    End Function
    Function Checkers(ByVal entry As String(), ByVal args As String) As Boolean
        ' TO DO NOT TEST to validate if the ENTRY length is less than 3, unless if args is get-matchers
        If args <> "get-matcher" Then
            If entry.Length < 3 Then
                Return False
            End If
        End If
        Dim col1() As String = {1, 2, 3}
        Dim col2() As String = {4, 5, 6}
        Dim col3() As String = {7, 8, 9}
        Dim Oblic1() As String = {col1(0), col2(1), col3(2)}
        Dim Oblic2() As String = {col1(2), col2(1), col3(0)}
        ' Cheking in line
        If compareArray(col1, entry, args) Then
            _tempCombinaison = col1
            Return True
        ElseIf compareArray(col2, entry, args) Then
            _tempCombinaison = col2
            Return True
        ElseIf compareArray(col3, entry, args) Then
            _tempCombinaison = col3
            Return True
        ElseIf compareArray(Oblic1, entry, args) Then
            _tempCombinaison = Oblic1
            Return True
        ElseIf compareArray(Oblic2, entry, args) Then
            _tempCombinaison = Oblic2
            Return True
        End If
        ' compare by going down
        For i = 0 To 2
            Dim str() As String = {col1(i), col2(i), col3(i)}
            If compareArray(str, entry, args) Then
                _tempCombinaison = str
                Return True
            End If
        Next
        Return False
    End Function
    Public Sub writePlayers()
        Label1.Text = playersName(0)
        Label2.Text = playersName(1)
        Label3.Text = player1Score
        Label4.Text = player2Score
        GroupBox1.Show()
    End Sub
    Sub sendMsg(ByVal msg As String, ByRef type As Integer)
        ' type = 0 is error || type = 1 is a success msg
        Label5.Text = msg
        Select Case type
            Case 0
                Label5.ForeColor = Color.Red
            Case 1
                Label5.ForeColor = Color.Green
        End Select
    End Sub
    Sub clear()
        Counter = 0
        changePlayer()
        player1Move = ""
        player2Move = ""
        Button1.Text = ""
        Button2.Text = ""
        Button3.Text = ""
        Button4.Text = ""
        Button5.Text = ""
        Button6.Text = ""
        Button7.Text = ""
        Button8.Text = ""
        Button9.Text = ""
    End Sub
    Sub clearMsg()
        Label5.Text = ""
        Label5.ForeColor = Color.Black
    End Sub
    '  MENU
    Private Sub AboutToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AboutToolStripMenuItem.Click
        MsgBox("Simple tic tac game wrote by parfait mutshipayi on 5/june/2017 ")
    End Sub
    Private Sub PlayWithCPUToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PlayWithCPUToolStripMenuItem.Click
	    ' I gonna write this part next time!!!!
        Me.Hide()
        form2.disable_input2()
        form2.ShowDialog()
    End Sub
    Private Sub TwoPlayersToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles TwoPlayersToolStripMenuItem.Click
        Me.Hide()
        form2.Show()
    End Sub
End Class