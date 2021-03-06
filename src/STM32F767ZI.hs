{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module STM32F767ZI
  ( module Atom.Language.Atom
  , compileProgram
  ) where

import Atom.Language.Atom
import NeatInterpolation (trimming)
import Data.Text (pack, unpack)

-- | Invoke the atom compiler.
compileProgram :: String -> Atom () -> IO ()
compileProgram pname program = do
  (schedule, _, _, _, _) <- compile pname defaults { cCode = prePostCode pname } program
  putStrLn $ reportSchedule schedule

prePostCode :: String -> [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode loopName _ _ _ =
  let loopName' = pack loopName
   in
  ( unpack $ [trimming|
      // --- THIS CODE IS AUTO-GENERATED BY ATOM ---
      
      #include "stm32f7xx_hal.h"
      #include "stm32f7xx_nucleo_144.h"

      /**
        * @brief  System Clock Configuration
        *         The system Clock is configured as follow :
        *            System Clock source            = PLL (HSE)
        *            SYSCLK(Hz)                     = 216000000
        *            HCLK(Hz)                       = 216000000
        *            AHB Prescaler                  = 1
        *            APB1 Prescaler                 = 4
        *            APB2 Prescaler                 = 2
        *            HSE Frequency(Hz)              = 8000000
        *            PLL_M                          = 8
        *            PLL_N                          = 432
        *            PLL_P                          = 2
        *            PLL_Q                          = 9
        *            PLL_R                          = 7
        *            VDD(V)                         = 3.3
        *            Main regulator output voltage  = Scale1 mode
        *            Flash Latency(WS)              = 7
        * @param  None
        * @retval None
        */
      static void SystemClock_Config(void) {
        RCC_ClkInitTypeDef RCC_ClkInitStruct;
        RCC_OscInitTypeDef RCC_OscInitStruct;

        /* Enable HSE Oscillator and activate PLL with HSE as source */
        RCC_OscInitStruct.OscillatorType = RCC_OSCILLATORTYPE_HSE;
        RCC_OscInitStruct.HSEState = RCC_HSE_BYPASS;
        RCC_OscInitStruct.HSIState = RCC_HSI_OFF;
        RCC_OscInitStruct.PLL.PLLState = RCC_PLL_ON;
        RCC_OscInitStruct.PLL.PLLSource = RCC_PLLSOURCE_HSE;
        RCC_OscInitStruct.PLL.PLLM = 8;
        RCC_OscInitStruct.PLL.PLLN = 432;
        RCC_OscInitStruct.PLL.PLLP = RCC_PLLP_DIV2;
        RCC_OscInitStruct.PLL.PLLQ = 9;
        RCC_OscInitStruct.PLL.PLLR = 7;
        if(HAL_RCC_OscConfig(&RCC_OscInitStruct) != HAL_OK) {
          while(1) {};
        }

        /* Activate the OverDrive to reach the 216 Mhz Frequency */
        if(HAL_PWREx_EnableOverDrive() != HAL_OK) {
          while(1) {};
        }

        /* Select PLL as system clock source and configure the HCLK, PCLK1 and PCLK2
        clocks dividers */
        RCC_ClkInitStruct.ClockType =
          (RCC_CLOCKTYPE_SYSCLK | RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2);
        RCC_ClkInitStruct.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
        RCC_ClkInitStruct.AHBCLKDivider = RCC_SYSCLK_DIV1;
        RCC_ClkInitStruct.APB1CLKDivider = RCC_HCLK_DIV4;
        RCC_ClkInitStruct.APB2CLKDivider = RCC_HCLK_DIV2;
        if(HAL_RCC_ClockConfig(&RCC_ClkInitStruct, FLASH_LATENCY_7) != HAL_OK) {
          while(1) {};
        }
      }

      /**
        * @brief  CPU L1-Cache enable.
        */
      static void CPU_CACHE_Enable(void) {
        /* Enable I-Cache */
        SCB_EnableICache();

        /* Enable D-Cache */
        SCB_EnableDCache();
      }

      /**
        * @brief GPIO initialization structure
        */
      static GPIO_InitTypeDef  GPIO_InitStruct;

      /**
        * @brief toggle LED1
        */
      void toggleLED1() {
        HAL_GPIO_TogglePin(GPIOB, GPIO_PIN_0);
      }
    |]
  , unpack $ [trimming|
      int main(int argc, char* argv[]) {
        /* Enable the CPU Cache */
        CPU_CACHE_Enable();

        /* STM32F7xx HAL library initialization:
            - Configure the Flash prefetch
            - Systick timer is configured by default as source of time base, but user
              can eventually implement his proper time base source (a general purpose
              timer for example or other time source), keeping in mind that Time base
              duration should be kept 1ms since PPP_TIMEOUT_VALUEs are defined and
              handled in milliseconds basis.
            - Set NVIC Group Priority to 4
            - Low Level Initialization
        */
        HAL_Init();

        /* Configure the system clock to 216 MHz */
        SystemClock_Config();

        /* Enable GPIO Clock (to be able to program the configuration registers) */
        __HAL_RCC_GPIOB_CLK_ENABLE();

        /* Configure IO in output push-pull mode to drive external LEDs */
        GPIO_InitStruct.Mode  = GPIO_MODE_OUTPUT_PP;
        GPIO_InitStruct.Pull  = GPIO_PULLUP;
        GPIO_InitStruct.Speed = GPIO_SPEED_FREQ_VERY_HIGH;

        GPIO_InitStruct.Pin = GPIO_PIN_0;
        HAL_GPIO_Init(GPIOB, &GPIO_InitStruct);

        /* Run program loop */
        while(1) {
          ${loopName'}();
          HAL_Delay(1);  // Sleep for 1 ms
        }
      }

      // --- END OF AUTO-GENERATED CODE ---
    |]
  )
