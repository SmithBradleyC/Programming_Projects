Population test;
PVector goal  = new PVector(400, 10);
PVector eat1 = new PVector(200, 600);
PVector eat2 = new PVector(800, 400);



void setup() {
  size(1000, 1000); //size of the window
  frameRate(140000);//increase this to make the dots go faster
  test = new Population(500);//create a new population with 1000 members
}


void draw() { 
  background(255);

  //draw goal
  fill(255, 0, 0);
  ellipse(goal.x, goal.y, 10, 10);
  
  //draw eat1
  fill(204, 102, 0);
  ellipse(eat1.x, eat1.y, 20, 20);
  
  //draw eat2
  fill(204, 102, 0);
  ellipse(eat2.x, eat2.y, 20, 20);

  //draw obstacle(s)
  fill(0, 0, 255);
  rect(0, 300, 600, 10);
  
  fill(204,102,0);
  rect(400, 500, 600, 10);
  
  fill(0, 0, 255);
  rect(0, 700, 600, 10);


  if (test.allDotsDead()) {
    //genetic algorithm
    test.calculateFitness();
    test.naturalSelection();
    test.mutateDemBabies();
  } else {
    //if any of the dots are still alive then update and then show them

    test.update();
    test.show();
  }
}
