unit sllist;

interface

type
  TslList = class
  public
    Ownobjects: Boolean;
    items: array of TObject;
    Count: Integer;
    constructor Create(OwnObjects: Boolean = True);
    destructor Destroy; override;
    function IndexOf(const x: TObject): Integer;
    procedure Add(const x: TObject);
    procedure Remove(const x: TObject); overload;
    procedure Remove(const index: Integer); overload;
    procedure Clear;
  end;

  TslList2 = class
  public
    list1, list2: TslList;
    constructor Create(OwnObjects: Boolean = True);
    destructor Destroy; override;
    function IndexOf(const x1, x2: TObject): Integer;
    procedure Add(const x1, x2: TObject);
    procedure Remove(const x1, x2: TObject); overload;
    procedure Remove(const index: Integer); overload;
    procedure Clear;
  end;


implementation

{ TslList }

procedure TslList.Add(const x: TObject);
begin
  inc(Count);
  SetLength(items, Count);
  items[Count-1]:= x;
end;

procedure TslList.Clear;
var i: Integer;
begin
  if OwnObjects then
    for i:= 0 to Count -1 do
      if items[i] <> nil then
        items[i].Free;

  Count:= 0;
  SetLength(items, Count);
end;

constructor TslList.Create(OwnObjects: Boolean=True);
begin
  self.OwnObjects:= OwnObjects
end;

destructor TslList.Destroy;
begin
  Clear;
  inherited;
end;

function TslList.IndexOf(const x: TObject): Integer;
var i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count -1 do
    if items[i] = x then
    begin
      Result:= i;
      exit;
    end;
end;

procedure TslList.Remove(const x: TObject);
begin
  Remove(IndexOf(x));
end;

procedure TslList.Remove(const index: Integer);
var i: Integer;
begin
  if ((index >= Count) or (index < 0)) then exit;

  if OwnObjects then
    if items[index] <> nil then
      items[index].Free;

  for i:= index to Count -2 do
    items[i]:= items[i+1];
  dec(Count);
  SetLength(items, Count);
  
end;

{ TslList2 }

procedure TslList2.Add(const x1, x2: TObject);
begin
  list1.Add(x1);
  list2.Add(x2);
end;

constructor TslList2.Create(OwnObjects: Boolean=  True);
begin
  list1:= TslList.Create(OwnObjects);
  list2:= TslList.Create(OwnObjects);  
end;

destructor TslList2.Destroy;
begin
  list1.Free;
  list2.Free;

  inherited;
end;

function TslList2.IndexOf(const x1, x2: TObject): Integer;
var i: Integer;
begin
  Result:= -1;
  for i:= 0 to list1.Count -1 do
  begin
    if ((list1.items[i] = x1) and (list2.items[i] = x2)) then
    begin
      Result:= i;
      exit;
    end;
  end;
end;

procedure TslList2.Remove(const x1, x2: TObject);
begin
  Remove(IndexOf(x1,x2));
end;

procedure TslList2.Remove(const index: Integer);
begin
  list1.Remove(index);
  list2.Remove(index);  
end;

procedure TslList2.Clear;
begin
  list1.Clear;
  list2.Clear;
end;

end.
