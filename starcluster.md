You'll need to have Python and easy_install on your system.
On ubuntu, I ran
```
sudo apt-get install python-setuptools
```
Then you can install StarCluster as follows:
```
sudo easy_install StarCluster
```
To get started, you'll need to set up a configuration file.
StarCluster will help you do this if you enter the following:
```
starcluster help
```
in which case you'll get the following output:
```
StarCluster - (http://star.mit.edu/cluster) (v. 0.95.6)
Software Tools for Academics and Researchers (STAR)
Please submit bug reports to starcluster@mit.edu

!!! ERROR - config file /home/fditraglia/.starcluster/config does not exist

Options:
--------
[1] Show the StarCluster config template
[2] Write config template to /home/fditraglia/.starcluster/config
[q] Quit

Please enter your selection: 
```
Enter 2 at this prompt.
You'll see this:
```
Please enter your selection: 2

>>> Config template written to /home/fditraglia/.starcluster/config
>>> Please customize the config template
```
StarCluster has created a text file for you in which you'll need to fill in your Amazon EC2 details.
You can open and edit this in your favorite text editor: it's located at ``~/.starcluster/config`` where ``~`` refers to your home directory.
(Mine is ``home/fditraglia/`` as you see from above.)
The dot in the file extension indicates that this is a hidden file, so you won't be able to see it in your file browser unless you've set it up to show hidden files.
Even if you haven't, you can always open and edit it at the command line, for example using the [nano](http://nano-editor.org):
```
nano ~/.starcluster/config
```
(Personally I much prefer editing text in [vim](http://www.vim.org) but I realize that it's not for everyone!)
The file is pretty long and contains a lot of information, but you can ignore most of it for the moment: we'll start off using the default cluster setup.
Before we can do this, you'll need to find the following portion of the text document:



Some Resources
--------------
-  [StarCluster and R](http://ronert-obst.com/blog/2013-09-01-starcluster.html)
-  [StarCluster Documentation](http://star.mit.edu/cluster/docs/latest/index.html)
-  [StarCluster Quick-Start](http://star.mit.edu/cluster/docs/latest/quickstart.html)
