class Dot {
  PVector pos;
  PVector vel;
  PVector acc;
  Brain brain;

  boolean dead = false;
  boolean oldAge = false;
  boolean reachedGoal = false;
  boolean reachedEat1 = false;
  boolean reachedEat2 = false;
  boolean isBest = false;//true if this dot is the best dot from the previous generation

  float fitness = 0;
  float RG = 0; // reached goal
  float RE1 = 0; // reached eat1
  float RE2 = 0; // reached eat2
  float D = 0; // dead
  float OA = 0; // reached old age
  float distanceToGoal = 10000; // impossible distance
  int brainSize = 1000;
  

  Dot() {
    brain = new Brain(1000);//new brain with 1000 instructions

    //start the dots at the bottom of the window with a no velocity or acceleration
    pos = new PVector(width/2, height- 10);
    vel = new PVector(0, 0);
    acc = new PVector(0, 0);
  }


  //-----------------------------------------------------------------------------------------------------------------
  //draws the dot on the screen
  void show() {
    //if this dot is the best dot from the previous generation then draw it as a big green dot
    if (isBest) {
      fill(0, 255, 0);
      ellipse(pos.x, pos.y, 8, 8);
    } else {//all other dots are just smaller black dots
      fill(0);
      ellipse(pos.x, pos.y, 4, 4);
    }
  }

  //-----------------------------------------------------------------------------------------------------------------------
  //moves the dot according to the brains directions
  void move() {

    if (brain.directions.length > brain.step) {//if there are still directions left then set the acceleration as the next PVector in the direcitons array
      //if(brain.directions.length/2 > brain.step || reachedEat2){       // added
        //if(brain.directions.length/3 >brain.step || reachedEat1){  // added
          acc = brain.directions[brain.step];
          brain.step++;
        //}  // added
      //}      // added
    } else {//if at the end of the directions array then the dot is dead
      dead = true;
      //oldAge = true;
    }

    //apply the acceleration and move the dot
    vel.add(acc);
    vel.limit(5);//not too fast
    pos.add(vel);
  }

  //-------------------------------------------------------------------------------------------------------------------
  //calls the move function and check for collisions and stuff
  void update() {
    if (!dead && !reachedGoal) {
      move();
      if (pos.x< 2|| pos.y<2 || pos.x>width-2 || pos.y>height -2) {//if near the edges of the window then kill it 
        dead = true;
      } else if (dist(pos.x, pos.y, goal.x, goal.y) < 5) {//if reached goal
        reachedGoal = true;
      }else if (dist(pos.x,pos.y,eat1.x,eat1.y)<10){
        reachedEat1 = true;
      }else if (dist(pos.x,pos.y,eat2.x,eat2.y)<10){
        reachedEat2 = true;
      }else if (pos.x< 600 && pos.y < 310 && pos.x > 0 && pos.y > 300) {//if hit obstacle
        dead = true;
      }else if (pos.x< 1000 && pos.y < 510 && pos.x > 400 && pos.y > 500) {//if hit obstacle
        dead = true;
      }else if (pos.x< 600 && pos.y < 710 && pos.x > 0 && pos.y > 700) {//if hit obstacle
        dead = true;
      }
    }
  }

  //--------------------------------------------------------------------------------------------------------------------------------------
  void distToGoal(){
    float Cor1_Cor2 = dist(600,700,400,500);
    float Cor2_Cor3 = dist(400,500,600,300);
    float Cor3_Dest = dist(600,300,goal.x,goal.y);
    
    
    if(pos.y>700){
      distanceToGoal = dist(pos.x,pos.y,600,700)+Cor1_Cor2+Cor2_Cor3+Cor3_Dest;
    }else if(pos.y>500){
      distanceToGoal = dist(pos.x,pos.y,400,500)+Cor2_Cor3+Cor3_Dest;
    }else if(pos.y>300){
      distanceToGoal = dist(pos.x,pos.y,600,300)+Cor3_Dest;
    }else{distanceToGoal = dist(pos.x,pos.y,goal.x,goal.y);}
  }
    
    
  //calculates the fitness
  void calculateFitness() {
    
    //Brad's new fitness
    if(reachedGoal){RG = 1;}
    if(reachedEat1){RE1 = 1;}
    if(reachedEat2){RE2 = 1;}
    if(dead){D = 1;}
    if(oldAge){OA = 1;}
    distToGoal();
    //float distanceToGoal = dist(pos.x,pos.y,goal.x,goal.y);
    
    fitness = RG*(1/16+10000/(float)(brain.step*brain.step)) + D*(1/(distanceToGoal*distanceToGoal));
    /*
    fitness =((float)(brain.step*brain.step)/(brainSize*brainSize) + 
               //RE1*(brainSize*brainSize)/4 + 
               //RE2*(brainSize*brainSize)/4 +
               RG*(brainSize*brainSize)/2) +
               1000/distanceToGoal;
        */
    /*
    // original fitness
    if (reachedGoal) {//if the dot reached the goal then the fitness is based on the amount of steps it took to get there
      fitness = 1.0/16.0 + 10000.0/(float)(brain.step * brain.step);
    } else {//if the dot didn't reach the goal then the fitness is based on how close it is to the goal
      float distanceToGoal = dist(pos.x, pos.y, goal.x, goal.y);
      fitness = 1.0/(distanceToGoal * distanceToGoal);
    }
    */
  }

  //---------------------------------------------------------------------------------------------------------------------------------------
  //clone it 
  Dot gimmeBaby() {
    Dot baby = new Dot();
    baby.brain = brain.clone();//babies have the same brain as their parents
    return baby;
  }
}
