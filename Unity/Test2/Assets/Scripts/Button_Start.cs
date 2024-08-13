using ARETT.JSON;
using ARETT;
using Microsoft.MixedReality.Toolkit.Utilities;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;
using Unity.VisualScripting;


public class Button_Start : MonoBehaviour
{
    public GameObject start_timer;
    public GameObject End_Button;
    public GameObject snake;
    public GameObject ferry;
    public GameObject nerfgun;
    public GameObject start;

    public void Start_Task()
    {
    // Sets Name for the log file into that of the actual task
    TaskManager.Instance.logger.RecordingName = TaskManager.Instance.task;
         // Creates Timer in the HoloLens Display
    Instantiate(start_timer, Camera.main.transform);
        // Creates the GameObject of the building bricks model
        if (TaskManager.Instance.task == "Steamboat")
        {
            
        }
        else if (TaskManager.Instance.task == "Dog")
        {

        }
        else if (TaskManager.Instance.task == "Nerfgun")
        {
            GameObject nerfgun2 = Instantiate<GameObject>(nerfgun, Camera.main.transform);
            nerfgun2.transform.parent = null;
        }
        else if (TaskManager.Instance.task == "Snake")
        {
            GameObject snake2 = Instantiate<GameObject>(snake, Camera.main.transform);
            snake2.transform.parent = null;
        }
        else if (TaskManager.Instance.task == "Ferry")
        {
            GameObject ferry2 = Instantiate<GameObject>(ferry, Camera.main.transform);
            ferry2.transform.parent = null;

        }

        if (TaskManager.Instance.logger.FileHandler.writingData)
        {
            Debug.LogError("[EyeTracking] Already recording");

        }
        else
        {
            // Queue to start recording
            TaskManager.Instance.logger.StartRecording();
            Debug.LogError("Started recording");

        }
        // Creates End Button
        Instantiate(End_Button, Camera.main.transform);
    }
    public void New_Start_Button()
    {
        // Creates Start Button again
        GameObject task2 = Instantiate<GameObject>(start, Camera.main.transform);


    }
}
