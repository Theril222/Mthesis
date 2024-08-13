using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Anchor_Handler : MonoBehaviour
{
    public GameObject anleitung;
 
    public void anchor_Experiment()
    {
        Instantiate(anleitung, Camera.main.transform);
    }
}
