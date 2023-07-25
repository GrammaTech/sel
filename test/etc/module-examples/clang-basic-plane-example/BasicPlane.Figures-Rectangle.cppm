export module BasicPlane.Figures:Rectangle; // defines the module partition Rectangle

import :Point;

export struct Rectangle // make this struct visible to importers
{
  Point ul, lr;
};

export int height(const Rectangle& r) { return r.ul.y - r.lr.y; }
export int width(const Rectangle& r) { return r.lr.x - r.ul.x; }
export int area(const Rectangle& r) { return width(r) * height(r); }
