Public Class form2
    Dim disable_textbox2 As Boolean = False
    Public Sub disable_input2()
        TextBox2.Dispose()
        Label3.Text = ""
        Label2.Text = "Your name"
        disable_textbox2 = True
    End Sub
    Private Sub form2_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.Text = "New game"
    End Sub
    Private Sub onClose(sender As Object, e As EventArgs) Handles MyBase.FormClosed
        Form1.Show()
        Form1.sendMsg("Operation canceled", 0)
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim on_success As Boolean = False
        If disable_textbox2 Then
            ' Play with the CPU
            If TextBox1.Text <> "" Then
                on_success = True
                Form1.playersName(0) = TextBox1.Text
                Form1.playersName(1) = "CPU"
            Else
                MsgBox("Please type your name ")
            End If
        Else
            ' 2 players mode
            If TextBox1.Text <> Nothing And TextBox2.Text <> Nothing Then
                on_success = True
                Form1.playersName(0) = TextBox1.Text
                Form1.playersName(1) = TextBox2.Text
            Else
                MsgBox("Please fill all inputs ")
            End If
        End If
        If on_success Then
            Me.Hide()
            Form1.Show()
            Form1.clear()
            Form1.sendMsg("game started!!!", 1)
            Form1.isGameRuning = True
            Form1.player1Score = 0
            Form1.player2Score = 0
            Form1.writePlayers()
        End If
    End Sub
End Class