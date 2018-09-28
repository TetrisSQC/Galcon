{$DEFINE AUDIO}
unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, UGame
{$IFDEF AUDIO}, Bass{$ENDIF};

const
  levelData: array[0..11, 0..2] of single = (
  // nodes, teams, ai delay
    (5, 2, 4),
    (6, 2, 3.5),
    (8, 2, 3),
    (10, 2, 3),
    (9, 3, 4),
    (12, 3, 3.5),
    (15, 3, 3),
    (6, 3, 2.5),
    (16, 4, 3),
    (18, 4, 2.5),
    (12, 4, 2),
    (20, 4, 1.5)
    );

type
  TTween = class
  private
    FDelay: Cardinal;
    FAnimate: Boolean;
    FAlpha: single;
    FColor: Cardinal;
    FFadeOut: Boolean;
    FStart: Cardinal;
    FVisible: Boolean;
    FOnAnimationEnd: TNotifyEvent;
  public
    procedure Show(const color: Cardinal);
    procedure Hide(Delay: Cardinal = 0);

    procedure Animate;

    property Alpha: single read FAlpha;
    property Color: Cardinal read FColor;
    property Visible: Boolean read FVisible;
    property OnAnimationEnd: TNotifyEvent read FOnAnimationEnd
      write FOnAnimationEnd;
  end;

  TfrmMain = class(TForm, IDisplay)
    Timer1: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private-Deklarationen }
    FLevel: integer;
    FMouse: TPoint;
    FMouseDown: Boolean;
    FGame: TGameScene;
    FTween: TTween;
    FLastTick: Cardinal;
    FProgram: Cardinal;
    FStart: Cardinal;
{$IFDEF AUDIO}
    FMusicChannel: Cardinal;
    FHit: array[0..5] of Cardinal;
    FLaunch: Cardinal;
    FPing: array[0..2] of Cardinal;

    procedure LoadEffect(Filename: string; var Handle: Cardinal);

{$ENDIF}
    procedure DrawLine(x1, y1, x2, y2: single; color: cardinal;
      thickness: single = 2; alpha: single = 1);
    procedure DrawQuad(x1, y1, x2, y2: single; color: cardinal;
      alpha: single = 1);
    procedure drawCircle(x, y, outer_radius, inner_radius: single;
      color: cardinal; glow: Boolean = false; alpha: single = 1.0;
      segments: integer = 32);

    procedure GetMouse(out x, y: single);
    procedure GetDimension(out Width, Height: single);
    procedure PlaySound(const sample: string);
    procedure DoAnimationEnd(Sender: TObject);

    function LoadShader(const fragmentShaderString: string): Boolean;

  public
    { Public-Deklarationen }
    procedure StartGame;
  end;

var
  frmMain: TfrmMain;

implementation uses dglOpengl;

{$R *.dfm}

const
  vertexShaderString = 'void main(void) { ' + sLineBreak +
    'gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;}';

  pixelShaderString =
    '    uniform vec3 iResolution;' + slineBreak +
    'uniform float iTime;' + slineBreak +
    '#define ANIMATE' + slineBreak +
    '#define OCTAVES 5' + slineBreak +
    'vec3 mod289(vec3 x)' + slineBreak +
    '{' + slineBreak +
    '  return x - floor(x * (1.0 / 289.0)) * 289.0;' + slineBreak +
    '}' + slineBreak +
    'vec2 mod289(vec2 x)' + slineBreak +
    '{' + slineBreak +
    '  return x - floor(x * (1.0 / 289.0)) * 289.0;' + slineBreak +
    '}' + slineBreak +
    'vec3 permute(vec3 x)' + slineBreak +
    '{' + slineBreak +
    '  return mod289(((x*34.0)+1.0)*x);' + slineBreak +
    '}' + slineBreak +
    'float snoise(vec2 v)' + slineBreak +
    '  {' + slineBreak +
    '  const vec4 C = vec4(0.211324865405187,  // (3.0-sqrt(3.0))/6.0' + slineBreak +
    '                      0.366025403784439,  // 0.5*(sqrt(3.0)-1.0)' + slineBreak +
    '                     -0.577350269189626,  // -1.0 + 2.0 * C.x' + slineBreak +
    '                      0.024390243902439); // 1.0 / 41.0' + slineBreak +
    '// First corner' + slineBreak +
    '  vec2 i  = floor(v + dot(v, C.yy) );' + slineBreak +
    '  vec2 x0 = v -   i + dot(i, C.xx);' + slineBreak +
    '// Other corners' + slineBreak +
    '  vec2 i1;' + slineBreak +
    '  //i1.x = step( x0.y, x0.x ); // x0.x > x0.y ? 1.0 : 0.0' + slineBreak +
    '  //i1.y = 1.0 - i1.x;' + slineBreak +
    '  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);' + slineBreak +
    '  // x0 = x0 - 0.0 + 0.0 * C.xx ;' + slineBreak +
    '  // x1 = x0 - i1 + 1.0 * C.xx ;' + slineBreak +
    '  // x2 = x0 - 1.0 + 2.0 * C.xx ;' + slineBreak +
    '  vec4 x12 = x0.xyxy + C.xxzz;' + slineBreak +
    '  x12.xy -= i1;' + slineBreak +
    '// Permutations' + slineBreak +
    '  i = mod289(i); // Avoid truncation effects in permutation' + slineBreak +
    '  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))' + slineBreak +
    '		+ i.x + vec3(0.0, i1.x, 1.0 ));' + slineBreak +
    '  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);' + slineBreak +
    '  m = m*m ;' + slineBreak +
    '  m = m*m ;' + slineBreak +
    '// Gradients: 41 points uniformly over a line, mapped onto a diamond.' + slineBreak +
    '// The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)' + slineBreak +
    '  vec3 x = 2.0 * fract(p * C.www) - 1.0;' + slineBreak +
    '  vec3 h = abs(x) - 0.5;' + slineBreak +
    '  vec3 ox = floor(x + 0.5);' + slineBreak +
    '  vec3 a0 = x - ox;' + slineBreak +
    '// Normalise gradients implicitly by scaling m' + slineBreak +
    '// Approximation of: m *= inversesqrt( a0*a0 + h*h );' + slineBreak +
    '  m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );' + slineBreak +
    '// Compute final noise value at P' + slineBreak +
    '  vec3 g;' + slineBreak +
    '  g.x  = a0.x  * x0.x  + h.x  * x0.y;' + slineBreak +
    '  g.yz = a0.yz * x12.xz + h.yz * x12.yw;' + slineBreak +
    '  return 130.0 * dot(m, g);' + slineBreak +
    '}' + slineBreak +
    'vec2 rand2(vec2 p)' + slineBreak +
    '{' + slineBreak +
    '    p = vec2(dot(p, vec2(12.9898,78.233)), dot(p, vec2(26.65125, 83.054543)));' + slineBreak +
    '    return fract(sin(p) * 43758.5453);' + slineBreak +
    '}' + slineBreak +
    'float rand(vec2 p)' + slineBreak +
    '{' + slineBreak +
    '    return fract(sin(dot(p.xy ,vec2(54.90898,18.233))) * 4337.5453);' + slineBreak +
    '}' + slineBreak +
    'vec3 hsv2rgb(vec3 c)' + slineBreak +
    '{' + slineBreak +
    '    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);' + slineBreak +
    '    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);' + slineBreak +
    '    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);' + slineBreak +
    '}' + slineBreak +
    '// Thanks to David Hoskins https://www.shadertoy.com/view/4djGRh' + slineBreak +
    'float stars(in vec2 x, float numCells, float size, float br)' + slineBreak +
    '{' + slineBreak +
    '    vec2 n = x * numCells;' + slineBreak +
    '    vec2 f = floor(n);' + slineBreak +
    '	float d = 1.0e10;' + slineBreak +
    '    for (int i = -1; i <= 1; ++i)' + slineBreak +
    '    {' + slineBreak +
    '        for (int j = -1; j <= 1; ++j)' + slineBreak +
    '        {' + slineBreak +
    '            vec2 g = f + vec2(float(i), float(j));' + slineBreak +
    '			g = n - g - rand2(mod(g, numCells)) + rand(g);' + slineBreak +
    '            // Control size' + slineBreak +
    '            g *= 1. / (numCells * size);' + slineBreak +
    '			d = min(d, dot(g, g));' + slineBreak +
    '        }' + slineBreak +
    '    }' + slineBreak +
    '    return br * (smoothstep(.95, 1., (1. - sqrt(d))));' + slineBreak +
    '}' + slineBreak +
    '// Simple fractal noise' + slineBreak +
    '// persistence - A multiplier that determines how quickly the amplitudes diminish for' + slineBreak +
    '// each successive octave.' + slineBreak +
    '// lacunarity - A multiplier that determines how quickly the frequency increases for' + slineBreak +
    '// each successive octave.' + slineBreak +
    'float fractalNoise(in vec2 coord, in float persistence, in float lacunarity)' + slineBreak +
    '{' + slineBreak +
    '    float n = 0.;' + slineBreak +
    '    float frequency = 1.;' + slineBreak +
    '    float amplitude = 1.;' + slineBreak +
    '    for (int o = 0; o < OCTAVES; ++o)' + slineBreak +
    '    {' + slineBreak +
    '        n += amplitude * snoise(coord * frequency);' + slineBreak +
    '        amplitude *= persistence;' + slineBreak +
    '        frequency *= lacunarity;' + slineBreak +
    '    }' + slineBreak +
    '    return n;' + slineBreak +
    '}' + slineBreak +
    'vec3 fractalNebula(in vec2 coord, vec3 color, float transparency)' + slineBreak +
    '{' + slineBreak +
    '    float n = fractalNoise(coord, .5, 2.);' + slineBreak +
    '    return n * color * transparency;' + slineBreak +
    '}' + slineBreak +
    'void mainImage(out vec4 fragColor, in vec2 fragCoord)' + slineBreak +
    '{' + slineBreak +
    '    float resolution = max(iResolution.y, iResolution.y);' + slineBreak +
    '    vec2 coord = fragCoord.xy / resolution;' + slineBreak +
    '    vec3 result = vec3(0.);' + slineBreak +
    '#ifdef ANIMATE' + slineBreak +
    '    vec3 nebulaColor1 = hsv2rgb(vec3(.5+.5*sin(iTime*.1), 0.5, .25));' + slineBreak +
    '	vec3 nebulaColor2 = hsv2rgb(vec3(.5+.5*sin(iTime*.21), 1., .25));' + slineBreak +
    '#else' + slineBreak +
    '    vec3 nebulaColor1 = hsv2rgb(vec3(.5, 0.5, .25));' + slineBreak +
    '    vec3 nebulaColor2 = hsv2rgb(vec3(.7, 1., .25));' + slineBreak +
    '#endif' + slineBreak +
    '    result += fractalNebula(coord + vec2(.1, .1), nebulaColor1, 1.);' + slineBreak +
    '    result += fractalNebula(coord + vec2(0., .2), nebulaColor2, .5);' + slineBreak +
    '    result += stars(coord, 4., 0.1, 2.) * vec3(.74, .74, .74);' + slineBreak +
    '    result += stars(coord, 8., 0.05, 1.) * vec3(.97, .74, .74);' + slineBreak +
    '    result += stars(coord, 16., 0.025, 0.5) * vec3(.9, .9, .95);' + slineBreak +
    '    fragColor = vec4(result, 1.);' + slineBreak +
    '}' + slineBreak +
    'void main(void) { mainImage(gl_FragColor, gl_FragCoord.xy); }';

function glSlang_GetInfoLog(glObject: Cardinal): Ansistring;
var
  blen, slen: GLInt;
  InfoLog: PGLCharARB;
begin
  glGetObjectParameterivARB(glObject, GL_OBJECT_INFO_LOG_LENGTH_ARB, @blen);
  if blen > 1 then
  begin
    GetMem(InfoLog, blen * SizeOf(GLCharARB));
    glGetInfoLogARB(glObject, blen, slen, InfoLog);
    Result := PAnsiChar(InfoLog);
    Dispose(InfoLog);
  end;
end;

function validateProgram(prog: GLUInt): Boolean;
var
  status: GLInt;
begin
  glValidateProgram(prog);
  glGetProgramiv(prog, GL_VALIDATE_STATUS, @status);
  Result := status = 1;
end;

function compileShader(typ: GLenum; shaderString: string): GLUInt;
var
  status: GLInt;
  sources: PGLchar;
  shader: GLUInt;
begin
  Result := 0;

  sources := PGLchar(Ansistring(shaderString));
  shader := glCreateShader(typ);
  if (shader = 0) or (shader = GL_INVALID_ENUM) then
    exit;

  glShaderSource(shader, 1, @sources, nil);
  glCompileShader(shader);

  glGetShaderiv(shader, GL_COMPILE_STATUS, @status);
  if status = 0 then
    showmessage(string(glSlang_GetInfoLog(shader)));

  if status = 0 then
    exit;
  Result := shader;
end;

{ TTween }

procedure TTween.Hide(Delay: Cardinal = 0);
begin
  FFadeOut := true;
  FStart := 0;
  FAnimate := true;
  FDelay := Delay;
end;

procedure TTween.Show(const color: Cardinal);
begin
  FColor := color;
  FFadeOut := false;
  FStart := 0;
  FAnimate := true;
  FVisible := true;
  FDelay := 0;
end;

procedure TTween.Animate;
begin
  if not FVisible then
    exit;
  if not FAnimate then
    exit;

  if FDelay > 0 then
  begin
    FDelay := FDelay - 1;
    exit;
  end;

  if FFadeOut then
    FAlpha := FAlpha - 0.1
  else
    FAlpha := FAlpha + 0.1;

  if FAlpha >= 1 then
  begin
    FAnimate := false;
    FAlpha := 1;
    if assigned(FOnAnimationEnd) then
      FOnAnimationEnd(self);
  end;

  if FAlpha < 0 then
  begin
    FAnimate := false;
    FVisible := false;
    if assigned(FOnAnimationEnd) then
      FOnAnimationEnd(self);
  end;
end;

{ TfrmMain }

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  wglMakeCurrent(Canvas.Handle, 0);
  FTween.free;
  FGame.free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  glViewPort(0, 0, ClientWidth, ClientHeight);
end;

{$IFDEF AUDIO}

procedure TfrmMain.LoadEffect(Filename: string; var Handle: Cardinal);
begin
  filename := 'Effects\' + Filename;
  Handle := BASS_SampleLoad(false,
    PWideChar(WideString(filename)),
    0, 0, 3,
    BASS_SAMPLE_OVER_POS or BASS_UNICODE);
end;
{$ENDIF}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  PixelFormat: Cardinal;
  PFD: pixelformatdescriptor;
  RC: HGLRC;
  shader: TStringlist;
{$IFDEF AUDIO}
  i: integer;
{$ENDIF}
begin
{$IFDEF AUDIO}
  if BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    FMusicChannel := BASS_MusicLoad(false,
      PWideChar(WideString('Effects\galaxyii.mod')),
      0, 0, BASS_STREAM_AUTOFREE or BASS_UNICODE, 44100);

    for i := 0 to 6 do loadEffect(format('hit0%d.mp3', [i + 1]), FHit[i]);
    for i := 0 to 2 do loadEffect(format('ping0%d.mp3', [i + 1]), FPing[i]);
    loadEffect('launch01.mp3', FLaunch);

    Bass_ChannelPlay(FMusicChannel, true);
  end;
{$ENDIF}
  RandSeed := GetTickCount;
  Randseed := 1123;
  with pfd do
  begin
    nSize := SizeOf(PIXELFORMATDESCRIPTOR);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW
      or PFD_SUPPORT_OPENGL
      or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 16; //Farbtiefe
    cRedBits := 0;
    cRedShift := 0;
    cGreenBits := 0;
    cBlueBits := 0;
    cBlueShift := 0;
    cAlphaBits := 0;
    cAlphaShift := 0;
    cAccumBits := 0; //Accumulation Buffer
    cAccumRedBits := 0;
    cAccumGreenBits := 0;
    cAccumBlueBits := 0;
    cAccumAlphaBits := 0;
    cDepthBits := 16; //Z-Buffer Tiefe
    cStencilBits := 0; //Stencil Buffer
    cAuxBuffers := 0;
    iLayerType := PFD_MAIN_PLANE;
    bReserved := 0;
    dwLayerMask := 0;
    dwVisibleMask := 0;
    dwDamageMask := 0
  end;
    //Pixel Format setzten
  PixelFormat := ChoosePixelFormat(Canvas.Handle, @pfd);
  SetPixelFormat(Canvas.Handle, PixelFormat, @pfd);

  RC := wglCreateContext(Canvas.Handle);
  ActivateRenderingContext(Canvas.Handle, RC);


  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  glEnable(GL_LINE_SMOOTH);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);

  FTween := TTween.Create;
  FTween.OnAnimationEnd := DoAnimationEnd;

  ClientWidth := 800;
  ClientHeight := 800;

  FGame := TGameScene.create(self);
  FLevel := 0;
  StartGame;

  shader := TStringlist.create;
  LoadShader(pixelShaderString);
end;


procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var node: TNode;
begin
  FMouse := Point(X, Y);
  if FMouseDown then
  begin
    node := FGame.getClosestNode(x, y);
    if assigned(node) and ((node.team = 1) or (node.captureTeam = 1)) then
      node.selected := true;
  end;
end;

procedure TfrmMain.DrawLine(x1, y1, x2, y2: single; color: cardinal;
  thickness: single = 2; alpha: single = 1);
var
  a, r, g, b: Single;
  w, h: single;
begin
  a := ((Color shr 24) and $FF) / 255;
  if (a <= 0) then
    exit;

  if alpha < 1 then
    a := a * alpha;

  r := ((Color shr 16) and $FF) / 255;
  g := ((Color shr 8) and $FF) / 255;
  b := ((Color and $FF)) / 255;

  glLineWidth(thickness);
  glBegin(GL_LINES);

  w := 2 / ClientWidth;
  h := 2 / ClientHeight;

  x1 := w * x1 - 1;
  x2 := w * x2 - 1;

  y1 := 1 - h * y1;
  y2 := 1 - h * y2;

  glColor4f(r, g, b, a);
  glVertex2f(x1, y1);

  glColor4f(r, g, b, a);
  glVertex2f(x2, y2);

  glEnd();
end;

procedure TfrmMain.DrawQuad(x1, y1, x2, y2: single; color: cardinal;
  alpha: single = 1);
var
  a, r, g, b: Single;
  w, h: Single;
begin
  a := ((Color shr 24) and $FF) / 255;
  if (a <= 0) or (Opaque < 0) then
    exit;

  if alpha < 1 then
    a := a * alpha;

  r := ((Color shr 16) and $FF) / 255;
  g := ((Color shr 8) and $FF) / 255;
  b := ((Color and $FF)) / 255;

  w := 2 / ClientWidth;
  h := 2 / ClientHeight;

  x1 := w * x1 - 1;
  x2 := w * x2 - 1;
  y1 := 1 - h * y1;
  y2 := 1 - h * y2;

  glColor4F(r, g, b, a);

  glBegin(GL_TRIANGLE_STRIP);
  glVertex2F(x1, y2);
  glVertex2F(x1, y1);
  glVertex2F(x2, y1);
  glVertex2F(x1, y2);
  glVertex2F(x2, y2);
  glVertex2F(x2, y1);
  glEnd();
end;

procedure TfrmMain.drawCircle(x, y, outer_radius, inner_radius: single;
  color: cardinal; glow: Boolean = false; alpha: single = 1.0;
  segments: integer = 32);
var
  a, r, g, b: Single;
  w, h: Single;
  x1, y1, x2, y2: Single;
  step, angle: single;
  i: integer;
begin
  a := ((Color shr 24) and $FF) / 255;
  if (a <= 0) or (Opaque < 0) then
    exit;

  if alpha < 1 then
    a := a * alpha;

  r := ((Color shr 16) and $FF) / 255;
  g := ((Color shr 8) and $FF) / 255;
  b := ((Color and $FF)) / 255;

  w := 2 / ClientWidth;
  h := 2 / ClientHeight;

  angle := 0;
  step := PI * 2 / segments;

  glBegin(GL_TRIANGLE_STRIP);
  for i := 0 to segments do
  begin
    x1 := (x + cos(angle) * outer_radius) * w - 1;
    y1 := 1 - (y + sin(angle) * outer_radius) * h;
    x2 := (x + cos(angle) * inner_radius) * w - 1;
    y2 := 1 - (y + sin(angle) * inner_radius) * h;

    if glow then
      glColor4F(r, g, b, 0) else
      glColor4F(r, g, b, a);

    glVertex2d(x1, y1);
    glColor4F(r, g, b, a);
    glVertex2d(x2, y2);

    angle := angle + step;
  end;
  glEnd();


end;

procedure TfrmMain.GetMouse(out x, y: single);
begin
  X := FMouse.X;
  Y := FMouse.Y;
end;

procedure TfrmMain.GetDimension(out Width, Height: single);
begin
  Width := ClientWidth;
  Height := ClientHeight;
end;

procedure TFrmMain.PlaySound(const sample: string);
{$IFDEF AUDIO}
var
  handle: Cardinal;
  ch: HCHANNEL;
begin
  if Sample = 'hit01' then handle := FHit[0]
  else if Sample = 'hit02' then handle := FHit[1]
  else if Sample = 'hit03' then handle := FHit[2]
  else if Sample = 'hit04' then handle := FHit[3]
  else if Sample = 'hit05' then handle := FHit[4]
  else if Sample = 'hit06' then handle := FHit[5]
  else if Sample = 'launch01' then handle := FLaunch
  else if Sample = 'ping01' then handle := FPing[0]
  else if Sample = 'ping02' then handle := FPing[0]
  else if Sample = 'ping03' then handle := FPing[0] else exit;
  ch := BASS_SampleGetChannel(handle, FALSE);
  BASS_ChannelSetAttribute(ch, BASS_ATTRIB_VOL, 0.2);
  BASS_ChannelPlay(ch, false);
{$ELSE}
begin
{$ENDIF}
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  node: TNode;
begin
  FMouseDown := true;
  node := FGame.getClosestNode(x, y);
  if assigned(node) and ((node.team = 1) or (node.captureTeam = 1)) then
    node.selected := true;
end;

procedure TfrmMain.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := false;
  FGame.sendShips();
end;

procedure TfrmMain.StartGame;
begin
  FGame.Init(trunc(levelData[FLevel][0]),
    trunc(levelData[FLevel][1]),
    trunc(levelData[FLevel][2]));
end;

procedure TfrmMain.DoAnimationEnd(Sender: TObject);
begin
  if FTween.Visible then
  begin
    FTween.Hide(20);
    if FGame.TeamWon = 1 then inc(FLevel);
    StartGame;
  end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
var tick: Cardinal;
begin
  tick := GetTickCount;
  FGame.Update((tick - FLastTick) / 1000);
  FLastTick := tick;

  if (FGame.GameOver) and (not FTween.Visible) then
    FTween.Show(gamecolors[FGame.TeamWon]);

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  if FProgram <> 0 then
  begin
    glUseProgram(FProgram);
    glUniform1f(glGetUniformLocation(FProgram, 'iTime'),
      (GetTickCount - FStart) / 1000);
    glUniform3f(glGetUniformLocation(FProgram, 'iResolution'), ClientWidth,
      ClientHeight, 0);

    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex3f(-1.0, -1.0, 0.0);
    glTexCoord2f(ClientWidth, 0);
    glVertex3f(1.0, -1.0, 0.0);
    glTexCoord2f(ClientWidth, ClientHeight);
    glVertex3f(1.0, 1.0, 0.0);
    glTexCoord2f(0, ClientHeight);
    glVertex3f(-1.0, 1.0, 0.0);
    glEnd();
    glUseProgram(0);
  end;

  Fgame.Render;

  if Ftween.Visible then
  begin
    FTween.Animate;
    DrawQuad(0, 0, ClientWidth, ClientHeight, FTween.Color, FTween.Alpha);
  end;

  SwapBuffers(Canvas.Handle);
end;


function TfrmMain.LoadShader(const fragmentShaderString: string): Boolean;
var
  vertShader, fragShader: GLUInt;
  status: GLInt;
begin
  Result := false;

  if FProgram <> 0 then // Already Loaded
  begin
    glDeleteProgram(FProgram);
    FProgram := 0;
  end;

  if fragmentShaderString = '' then
  begin
    Result := true;
    exit;
  end;

  FStart := GetTickCount;

  vertShader := 0;
  fragShader := 0;

  FProgram := glCreateProgram();
  try
    vertShader := compileShader(GL_VERTEX_SHADER, vertexShaderString);
    if vertShader = 0 then
      exit;

    fragShader := compileShader(GL_FRAGMENT_SHADER, fragmentShaderString);
    if fragShader = 0 then
      exit;

    glAttachShader(FProgram, vertShader);
    glAttachShader(FProgram, fragShader);
    glLinkProgram(FProgram);

    glGetProgramiv(FProgram, GL_LINK_STATUS, @status);
    if status = 0 then
      exit;

    Result := validateProgram(FProgram);
  finally
    glDeleteShader(vertShader);
    glDeleteShader(fragShader);

    if not Result then
    begin
      glDeleteProgram(FProgram);
      FProgram := 0;
    end;
  end;
end;

initialization
  InitOpenGl;
end.

