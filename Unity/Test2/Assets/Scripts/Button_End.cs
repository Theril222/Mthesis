using ARETT;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Button_End : MonoBehaviour
{
    public GameObject nasa;
    public GameObject end;
    public void End_Task()
    {
            

        if (!TaskManager.Instance.logger.FileHandler.writingData)
        {
            Debug.LogError("Can't stop recording as we currently aren't recording!");
        }
        else
        {
            // Queue to stop recording
            TaskManager.Instance.logger.StopRecording();

            Debug.LogError("Stopped recording.");

        }
        destroyLegoOnClick();
        destroyTimerOnClick();
        // Creates new Text Window NASA
        GameObject nasa2 = Instantiate<GameObject>(nasa, Camera.main.transform);
        nasa2.transform.parent = null;
        // Sets remaining time to 300
        TaskManager.Instance.timeRemaining = 300;
        // Removes current Task from TaskList
        TaskManager.Instance.tasksList.Remove(TaskManager.Instance.task);
        // Selects new current Task or if the list is empty sets the current Task to End
        if (TaskManager.Instance.tasksList.Count != 0) {
            var random = new System.Random();
            int i = random.Next(TaskManager.Instance.tasksList.Count);
            TaskManager.Instance.task = TaskManager.Instance.tasksList[i];
        } else
        {
            TaskManager.Instance.task = "End";
        }


    }
    // Destroys Timer
    private void destroyTimerOnClick()
    {
        GameObject[] objectsToDestroy = GameObject.FindGameObjectsWithTag("Timer");
        foreach (GameObject go in objectsToDestroy)
        {
            Destroy(go);
        }
    }
    // Destroys Building Bricks model

    private void destroyLegoOnClick()
    {
        GameObject[] objectsToDestroy = GameObject.FindGameObjectsWithTag("LegoModell");
        foreach (GameObject go in objectsToDestroy)
        {
            Destroy(go);
        }
    }
    // Creates new End Button
    public void New_End_Button()
    {

        GameObject task2 = Instantiate<GameObject>(end, Camera.main.transform);
        

    }

}
