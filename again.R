system("git pull https://github.com/elenayang528/Shiny")

#Note that the system() function sends items to the Terminal. You can type in that line into a terminal without the system() wrapper and get the same result.

#Now What if you don't just want to bring in other repositories, but instead use your own so that you can quickly save code when needed?
#To do this you will need to tell RStudio and Github that this is your account and you have permissions to make changes to the repository.
#To do this we use a SSH key. These keys should not be posted anywhere and you should only call them when needed as they give access to your system and your git account.

system("git config --global user.email \"elena110645@gmail.com\"")  #Add your github email
system("git config --global user.name \"elenayang528\"")        #Add your github username
system("git config --list")                                       #Check your settings
"ssh-keygen -t rsa -C elena110645@gmail.com"                        #Generate your Keys
"cat ~/.ssh/id_rsa.pub"           