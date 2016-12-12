void setup(){
  size(400,400);
  center = new PVector(width/2,height/2);
  stroke(0,0,0,0);
  radius = width + height / 2;
  radius /= 5;
  size = width + height / 2;
  size /= 20;
  textFont(loadFont("MyanmarText-30.vlw"));
}
int n = 8;
int radius;
int size;
PVector center;
float spin = 0;
void draw(){
   spin+=.02;
  clear();
  background(255,255,255);
  
  for (int i=0; i < n; i++){
    fill(0,255 * i/n,255 * (n-i)/n);
    ellipse(center.x + radius * cos(2 * PI * i / n - (PI / 2) + spin), center.y + radius * sin(2 * PI * i / n - (PI / 2) + spin), size, size);
  }
  fill(0,0,0);
  text("Markov", center.x - width/8, center.y - width/15);
  text("Audio", center.x - width/8, center.y + width/15);
}