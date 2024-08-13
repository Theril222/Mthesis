using ARETT;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

public class TaskManager : MonoBehaviour
{
    public static TaskManager Instance;

    public float timeRemaining = 300;
    public DataLogger logger;
    public string task;
    public List<string> tasksList;



    void Awake()
    {
        Instance = this;
        string[] list = { "Steamboat", "Nerfgun", "Snake", "Ferry", "Dog" };
        this.tasksList = new List<string>(list);
        var random = new System.Random();
        int i = random.Next(tasksList.Count());
        this.task = tasksList[i];
        
    }
}
