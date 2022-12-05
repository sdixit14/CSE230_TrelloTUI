{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

import Actions
import Form
import Types
import Tasks
import Widgets
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit (assertEqual, testCase, assertFailure)
import Data.Text

main = defaultMain mainTree

mainTree = testGroup
             "all tests"
             [unitTests, failUnitTests]

unitTests =
  testGroup
    "Unit tests"
    [userSelector, taskSelector, tupleGenerator, selectMyTasks]

failUnitTests =
    expectFail
    (testGroup
        "Negative unit tests"
        [userFailSelector, userFailSelectorV2, taskFailSelector, taskFailSelectorV2])

test_tasks = [Task {_title = b, _content = b, _assignee = c} | c <- ["a", "b", "c", "d", "e", "f", "g", "h", "test_user", "test_user", "test_user"] 
                                                             |   b <- ["_", "_", "_", "_", "_", "_", "_", "_", "sample", "sample2", "sample3"]]
test_content = ["sample", "sample2", "sample3"]

getContentOutOfTasks = Prelude.map _content

test_users = ["test_user", "a", "b", "c"]
test_tuples = [("test_user", RadioField "test_user", "test_user"), ("a", RadioField "a", "a"), ("b", RadioField "b", "b"), ("c", RadioField "c", "c")]
test_workspaces = [Workspace {_name = "test_workspace", _users=test_users, _tasks=test_tasks}]

baseAppSt =  AppState{
                        _workspaces    = test_workspaces,
                        _workspace     = "test_workspace",
                        _user          = "test_user",
                        _dialogFlag    = True,
                        _taskFormFlag  = False,
                        _workspaceFormFlag = False,
                        _listTasksFlag = False,
                        _filterTasksFlag = False,
                        _dlg           = getDialog,
                        _taskForm      = emptyTaskForm,
                        _workspaceForm = emptyWorkspaceForm,
                        _persistFile   = "abc"
                    }

-- Pass test evaluating that users are found correctly
userSelector =
  testCase "Getting users out of an App State" $ assertEqual [] (test_users) (getCurrentUserList baseAppSt)
  
-- Two fail test evaluating that users are found correctly
userFailSelector = 
    testCase "Getting users out of an App State" $ assertEqual [] ("random":test_users) (getCurrentUserList baseAppSt)

userFailSelectorV2 = 
    testCase "Getting users out of an App State" $ assertEqual [] (Prelude.tail test_users) (getCurrentUserList baseAppSt)

-- Pass test evaluating that tasks are found correctly
taskSelector =
  testCase "Getting tasks out of an App State" $ assertEqual [] (test_tasks) (getCurrentTaskList baseAppSt)
  
-- Two fail test evaluating that users are found correctly
taskFailSelector = 
    testCase "Getting tasks out of an App State" $ assertEqual [] ((Prelude.head test_tasks):test_tasks) (getCurrentTaskList baseAppSt)

taskFailSelectorV2 = 
    testCase "Getting tasks out of an App State" $ assertEqual [] (Prelude.tail test_tasks) (getCurrentTaskList baseAppSt)

-- Pass test evaluating tuple generation
tupleGenerator =
  testCase "Generating a list of radio button tuples given a list of users" $ assertEqual [] (test_tuples) (generateListOfTuples test_users)

-- Pass test evaluating "filter tasks assigned to me"
-- also tests getMyWorkSpace
selectMyTasks =
  testCase "Generating a list of task contents assigned to me" $ assertEqual [] (test_content) (getContentOutOfTasks (getMyTasks baseAppSt (getMyWorkspace baseAppSt)))
