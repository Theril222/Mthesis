using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

public class TaskTitel2 : MonoBehaviour
{
    private TextMeshPro textMeshPRO;
    void Start()
    {
        textMeshPRO = gameObject.GetComponent<TextMeshPro>();
        if (TaskManager.Instance.task == "Steamboat")
        {
            textMeshPRO.text = "Aufgabe Steambot Teil 2/2";
        }
        else if (TaskManager.Instance.task == "Nerfgun")
        {
            textMeshPRO.text = "Aufgabe Nerfgun Teil 2/2";
        }
        else if (TaskManager.Instance.task == "Snake")
        {
            textMeshPRO.text = "Aufgabe Snake Teil 2/2";
        }
        else if (TaskManager.Instance.task == "Ferry")
        {
            textMeshPRO.text = "Aufgabe Ferry Teil 2/2";
        }
        else if (TaskManager.Instance.task == "Dog")
        {
            textMeshPRO.text = "Aufgabe Dog Teil 2/2";
        }
        else if (TaskManager.Instance.task == "End")
        {
            textMeshPRO.text = "Ende des Experiments";
        }
    }
}
