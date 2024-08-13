using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Task_Start : MonoBehaviour
{
    public GameObject start_button;

    public void Start_Experiment()
    {
        if (TaskManager.Instance.task != "End")
        {
            Instantiate(start_button, Camera.main.transform);
        }
        else
        {

        }

    }
}
