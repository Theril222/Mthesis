using ARETT;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Button_Nasa : MonoBehaviour
{
  
    public GameObject Task;

    public void Start_New_Task()
    {

        GameObject task2 = Instantiate<GameObject>(Task, Camera.main.transform);
        task2.transform.parent = null;


    }
}
