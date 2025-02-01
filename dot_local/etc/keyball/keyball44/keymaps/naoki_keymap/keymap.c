/*
Copyright 2022 @Yowkees
Copyright 2022 MURAOKA Taro (aka KoRoN, @kaoriya)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include QMK_KEYBOARD_H

#include "quantum.h"

// clang-format off
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  // keymap for default (VIA)
  [0] = LAYOUT_universal(
                         LGUI_T(KC_TAB) , KC_Q    , KC_W         , KC_E           , KC_R           , KC_T                 ,           KC_Y , KC_U           , KC_I         , KC_O    , KC_P          , KC_MINUS ,
                         KC_LCTL        , KC_A    , KC_S         , KC_D           , KC_F           , LT(3,KC_G)           ,           KC_H , LT(1,KC_J)     , LT(2,KC_K)   , KC_L    , KC_SCLN       , RALT_T(KC_QUOTE) ,
                         KC_LSFT        , KC_Z    , KC_X         , KC_C           , KC_V           , KC_B                 ,           KC_N , KC_M           , KC_COMM      , KC_DOT  , LT(3,KC_SLSH) , KC_BTN1  ,
                         LALT_T(KC_ESC) , KC_LGUI , LT(2,KC_SPC) , LT(1,KC_SPC)   , LSFT_T(KC_SPC)       ,                 RGUI_T(KC_BSPC) , LT(1,KC_ENT) , _______ , _______       , KC_RALT
                         ),

  [1] = LAYOUT_universal(
                         LGUI_T(KC_GRV) , KC_EXLM , KC_AT        , KC_HASH       , KC_DLR     , KC_PERC       ,  KC_CIRC , KC_AMPR  , KC_ASTR  , KC_LPRN , KC_RPRN      , KC_EQUAL         ,
                         KC_LCTL        , _______ , KC_BTN2      , KC_UP         , KC_BTN1       , KC_RGUI          ,  KC_LEFT       , KC_DOWN       , KC_UP        , KC_RGHT       , KC_LBRC     , RALT_T(KC_RBRC) ,
                         KC_LSFT        , _______ , KC_LEFT      , KC_DOWN       , KC_RIGHT      , RGUI(KC_RALT)    ,  _______       , KC_BTN1       , KC_BTN2      , KC_BTN2       , LT(3, KC_BSLS) , KC_BTN1          ,
                         LALT_T(KC_ESC) , KC_LGUI , LT(2,KC_SPC) , LT(1,KC_SPC)  , LSFT_T(KC_SPC)   ,                RGUI_T(KC_BSPC) , LT(1,KC_ENT) , _______       , _______             , KC_RALT
                         ),

  [2] = LAYOUT_universal(
                         KC_TILDE       , KC_1    , KC_2         , KC_3           , KC_4           , KC_5                     ,        KC_6    ,    KC_7 ,    KC_8 ,    KC_9 , KC_0                , KC_PLUS              ,
                         KC_LCTL        , _______ , _______      , _______        , KC_RGUI        , RSG(KC_RCTL)             ,        _______ ,    KC_4 ,    KC_5 ,    KC_6 , KC_LCBR , KC_RIGHT_CURLY_BRACE ,
                         KC_LSFT        , _______ , _______      , _______        , _______        , _______                  ,        KC_DOT  ,    KC_1 ,    KC_2 ,    KC_3 , KC_PIPE             , _______              ,
                         LALT_T(KC_ESC) , KC_LGUI , LT(2,KC_SPC) , LT(1,KC_SPC)   , LSFT_T(KC_SPC)           ,          RGUI_T(KC_BSPC) ,    KC_0 , _______ , _______             , _______
                         ),

  [3] = LAYOUT_universal(
                         LGUI_T(KC_TAB) , KC_F1   , KC_F2   , KC_F3   , KC_F4    , KC_F5                           ,        KC_F6    , KC_F7    , KC_F8   , KC_F9   , KC_F10  , KC_F11  ,
                         KC_LCTL        , KC_BRIU , KC_BRID , KC_MCTL , KC_MCTL  , _______                         ,        CPI_I100 , KC_MPRV  , KC_MPLY , KC_MNXT , KC_MUTE , KC_F12  ,
                         KC_LSFT        , _______ , KC_LEFT , KC_DOWN , KC_RIGHT , _______                         ,        CPI_D100 , KBC_SAVE , KC_BTN4 , KC_BTN5 , _______ , KC_BTN2 ,
                         _______        , KC_LGUI , _______ ,  _______ , _______                         ,                  KC_BSPC  , RGUI_T(KC_ENT) , _______ , _______ , _______
                         ),
};
// clang-format on

layer_state_t layer_state_set_user(layer_state_t state) {
    // Auto enable scroll mode when the highest layer is 3
    keyball_set_scroll_mode(get_highest_layer(state) == 3);
    return state;
}

#ifdef OLED_ENABLE

#include "lib/oledkit/oledkit.h"

void oledkit_render_info_user(void) {
    keyball_oled_render_keyinfo();
    keyball_oled_render_ballinfo();
    keyball_oled_render_layerinfo();
}
#endif

uint16_t get_quick_tap_term(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
        case LT(2,KC_SPC):
            return QUICK_TAP_TERM - 100;
        case LT(1,KC_SPC):
            return QUICK_TAP_TERM - 100;
        case LSFT_T(KC_SPC):
            return QUICK_TAP_TERM - 100;
        default:
            return QUICK_TAP_TERM;
    }
}
