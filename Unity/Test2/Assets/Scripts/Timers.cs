using ARETT.JSON;
using ARETT;
using Microsoft.MixedReality.Toolkit.Utilities;
using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;
using UnityEngine.UI;

public class Timers : MonoBehaviour
{
 
    private bool timerIsRunning = false;
    private TextMeshPro textMeshPRO;
    // public DataLogger datalogger = TaskManager.Instance.logger;


    private void Start()
    {
        textMeshPRO = gameObject.GetComponent<TextMeshPro>();
        timerIsRunning = true;
     
    }

    void Update()
    {
        if (timerIsRunning)
        {
            // Substract the remaining time and display as text
            if (TaskManager.Instance.timeRemaining > 0)
            {
                TaskManager.Instance.timeRemaining -= Time.deltaTime;
                textMeshPRO.text = DisplayTime(TaskManager.Instance.timeRemaining);
            }
            else
            {
                // Stop recording if out of time
                textMeshPRO.text = "Die Zeit ist abgelaufen!";
                TaskManager.Instance.timeRemaining = 0;
                timerIsRunning = false;
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
                Destroy(gameObject, 5);
            } 
        }

    }
    private string DisplayTime(float timeToDisplay)
    {
        timeToDisplay += 1;

        float minutes = Mathf.FloorToInt(timeToDisplay / 60);
        float seconds = Mathf.FloorToInt(timeToDisplay % 60);

        return string.Format("{0:00}:{1:00}", minutes, seconds);
    }

}


