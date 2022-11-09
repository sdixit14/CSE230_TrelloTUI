# TUI for Trello

### Overview
Trello is a task management application that allows multiple users to list tasks in a common workspace. It provides functionality to create a new task, assigning tasks to other members of the group, and many more. Objective of our project is to build a terminal user interface (TUI) for trello, a lightweight collaborative note-taking application. 

### Functionality
1. Allow users to create or join an existing workspace  
Similar to Trello, workspaces can be thought of as containers for tasks related to the same project or a group of people. Upon launch, the user would be presented with an option to either create a workspace or join an existing workspace.
    + Create workspace
    Upon selecting this option, the user would be provided with a form to enter the workspace details such as username, workspace name and a unique workspace ID.
    + Join existing workspace
    Upon selecting this option, the user would be provided with a form to enter their username and the ID of the existing workspace that they would like to join.
2. Allow users to add tasks to the workspace  
Once inside a workspace, the user would be able to see all the existing tasks present in that workspace. To add a new task, the user would be provided with a form to enter the title and description.
3. Allow users to assign a task to other members of the workspace  
While creating a task, the user can also assign the task to other members through a dropdown list containing the usernames of all the members present in the workspace. 
4. Allow users to filter tasks assigned to them   
In addition to viewing all the tasks, the user can also set a filter to view only the tasks that have been assigned to them.

### Components
Our application is composed of two modules
1. UI Layer   
We will utilize brick toolkit widgets such as forms, border, dialog-box, layer, and list to build the UI components of our application.
2. Data Persistence   
Since our application handles data in an asynchronous manner across multiple users, we would store data such as workspace id, usernames and task details in a text file or json file. We would also explore the option of using a database for data persistence if time permits. 

