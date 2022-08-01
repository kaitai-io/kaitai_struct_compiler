package io.kaitai.struct

import java.util.Locale

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

/**
  * Checks that our string case conversion utilities perform well in all locales.
  * To test with different locale than you have set up in your system, pass JVM
  * options, e.g. `-Duser.language=tr -Duser.country=TR`.
  */
class UtilsSpec extends AnyFunSpec with Matchers {
  val simpleStr = "foo_bar"
  val internalId = "_foo_bar"
  val allLetters = "a_b_c_d_e_f_g_h_i_j_k_l_m_n_o_p_q_r_s_t_u_v_w_x_y_z"

  describe("UtilsSpec") {
    it("checks default locale") {
      val l = Locale.getDefault
      Console.println(s"country: ${l.getCountry}")
      Console.println(s"language: ${l.getLanguage}")
      Console.println(s"displayScript: ${l.getDisplayScript}")
      Console.println(s"displayVariant: ${l.getDisplayVariant}")
    }

    describe("lowerUnderscoreCase") {
      it("has Turkish locale") {
        Console.println(Locale.getDefault)
        Console.println(Locale.forLanguageTag("tr"))
      }

      it("works with simple string") {
        Utils.lowerUnderscoreCase(simpleStr) shouldEqual "foo_bar"
      }

      it("is locale-independent") {
        Utils.lowerUnderscoreCase(allLetters) shouldEqual
          "a_b_c_d_e_f_g_h_i_j_k_l_m_n_o_p_q_r_s_t_u_v_w_x_y_z"
      }
    }

    describe("upperUnderscoreCase") {
      it("works with simple string") {
        Utils.upperUnderscoreCase(simpleStr) shouldEqual "FOO_BAR"
      }

      it("is locale-independent") {
        Utils.upperUnderscoreCase(allLetters) shouldEqual
          "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V_W_X_Y_Z"
      }
    }

    describe("upperCamelCase") {
      it("works") {
        Utils.upperCamelCase(simpleStr) shouldEqual "FooBar"
      }

      it("preserves initial underscore") {
        Utils.upperCamelCase(internalId) shouldEqual "_FooBar"
      }

      it("is locale-independent") {
        Utils.upperCamelCase(allLetters) shouldEqual
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      }
    }

    describe("lowerCamelCase") {
      it("works") {
        Utils.lowerCamelCase(simpleStr) shouldEqual "fooBar"
      }

      it("preserves initial underscore") {
        Utils.lowerCamelCase(internalId) shouldEqual "_fooBar"
      }

      it("is locale-independent") {
        Utils.lowerCamelCase(allLetters) shouldEqual
          "aBCDEFGHIJKLMNOPQRSTUVWXYZ"
      }
    }
  }
}
