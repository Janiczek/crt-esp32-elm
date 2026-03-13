#pragma once

#include "node.h"
#include "globals.h"
#include "graphics_extra.h"
#include "prelude.h"
#include "dirty.h"

void node_draw_tileXLine_inner(int x0, int x1, int y, uint8_t color, int tx0, int ty0) {
  int tx1 = tx0 + TILE_SIZE;
  int ty1 = ty0 + TILE_SIZE;
  // only draw if tile is not to the...
  if (tx1 >= x0 && tx0 <= x1 && ty1 >= y && ty0 <= y) {
    // right        left         top         bottom
    // of the line
    int cx0 = MAX(x0, tx0); // clamped x0
    int cx1 = MIN(x1, tx1); // clamped x1
    video.xLine(cx0,cx1,y,color);
  }
}

void node_draw_tileYLine_inner(int y0, int y1, int x, uint8_t color, int tx0, int ty0) {
  int tx1 = tx0 + TILE_SIZE;
  int ty1 = ty0 + TILE_SIZE;
  // only draw if tile is not to the...
  if (tx1 >= x && tx0 <= x && ty1 >= y0 && ty0 <= y1) {
    // right       left        top          bottom
    // of the line
    int cy0 = MAX(y0, ty0); // clamped y0
    int cy1 = MIN(y1, ty1); // clamped y1
    yLine(cy0,cy1,x,color);
  }
}

void node_draw_tileRect(Node* node, int tx0, int ty0) {
  uint8_t color = node->u.rect.color;
  int rx0 = node->u.rect.x;
  int ry0 = node->u.rect.y;
  int rx1 = rx0 + node->u.rect.w - 1;
  int ry1 = ry0 + node->u.rect.h - 1;
  node_draw_tileXLine_inner(rx0,rx1,ry0,color,tx0,ty0);
  node_draw_tileXLine_inner(rx0,rx1,ry1,color,tx0,ty0);
  node_draw_tileYLine_inner(ry0,ry1,rx0,color,tx0,ty0);
  node_draw_tileYLine_inner(ry0,ry1,rx1,color,tx0,ty0);
}

void node_draw_tileRectFill(Node* node, int tx0, int ty0) {
  // TODO: is it possible there's an off-by-one (eg. x+w-1 instead of x+w)?
  int tx1 = tx0 + TILE_SIZE;
  int ty1 = ty0 + TILE_SIZE;
  int rx0 = MAX(node->u.rect.x, tx0);
  int ry0 = MAX(node->u.rect.y, ty0);
  int rx1 = MIN(node->u.rect.x + node->u.rect.w, tx1);
  int ry1 = MIN(node->u.rect.y + node->u.rect.h, ty1);
  if (rx1 > rx0 && ry1 > ry0) 
    video.fillRect(rx0, ry0, rx1 - rx0, ry1 - ry0, node->u.rect.color);
}

void node_draw_tileXLine(Node* node, int tx0, int ty0) {
  int y = node->u.xline.y;
  int x0 = node->u.xline.x;
  int x1 = x0 + node->u.xline.len - 1;
  uint8_t color = node->u.xline.color;
  node_draw_tileXLine_inner(x0, x1, y, color, tx0, ty0);
}

void node_draw_tileYLine(Node* node, int tx0, int ty0) {
  int x = node->u.yline.x;
  int y0 = node->u.yline.y;
  int y1 = y0 + node->u.yline.len - 1;
  uint8_t color = node->u.yline.color;
  node_draw_tileYLine_inner(y0, y1, x, color, tx0, ty0);
}
