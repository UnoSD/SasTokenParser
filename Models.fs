module SasTokenParser.Models

type Tab =
    | Parser
    | About

type Model =
    {
        CurrentTab : Tab
        Url : string
        DisclaimerVisible : bool
    }
    
let emptyModel = 
    {
        CurrentTab = Parser
        Url = "https://myaccount.blob.core.windows.net/sascontainer/sasblob.txt?sv=2019-02-02&st=2019-04-29T22%3A18%3A26Z&se=2019-04-30T02%3A23%3A26Z&sr=b&sp=rw&sip=168.1.5.60-168.1.5.70&spr=https&sig=Z%2FRHIX5Xcg0Mq2rqI3OlWTjEg2tYkboXr1P9ZUXDtkk%3D"
        DisclaimerVisible = true
    }