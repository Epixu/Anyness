///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include <Langulus/NameOf-Runtime.hpp>


namespace Langulus::RTTI
{
   namespace Inner
   {
      /// Check if a name is reserved                                         
      bool IsReservedRt(const Token& name) {
         for (auto& reserved : ReservedKeywords) {
            if (name.size() != reserved.size())
               continue;

            size_t i = 0;
            for (; i < name.size(); ++i) {
               if (Langulus::ToLowercase(name[i]) != reserved[i])
                  break;
            }

            if (i == name.size())
               return true;
         }
         return false;
      }

      /// Analyze compiler stringification and find the left offset in order  
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the left           
      size_t CalculateTypeLeftOffsetRt() {
         ::std::string_view calibration_name
            = WrappedTypeNameRt<Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK>();

         #if LANGULUS_COMPILER(MSVC)
            // MSVC prepends "class "                                   
            size_t start = calibration_name.find(
               "class Langulus::RTTI::Inner::Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");
         #elif LANGULUS_COMPILER(GCC)
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            size_t start = calibration_name.find(
               "Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");
         #else
            size_t start = calibration_name.find(
               "Langulus::RTTI::Inner::Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");
         #endif

         return start;
      }
      
      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      size_t CalculateTypeRightOffsetRt() {
         ::std::string_view calibration_name = 
            WrappedTypeNameRt<Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK>();
         size_t start = CalculateTypeLeftOffsetRt();

         #if LANGULUS_COMPILER(MSVC)
            return calibration_name.size() - start - 67;
         #elif LANGULUS_COMPILER(GCC)
            return calibration_name.size() - start - 38;
         #else
            return calibration_name.size() - start - 61;
         #endif
      }

      /// Analyze compiler stringification and find the left offset in order  
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the left           
      size_t CalculateEnumLeftOffsetRt() {
         ::std::string_view calibration_name = 
            WrappedEnumNameRt<Oddly_Specific_EnumASDOLSAJDPAFHOAF>();

         size_t start = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_EnumASDOLSAJDPAFHOAF");
         return start;
      }

      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      size_t CalculateEnumRightOffsetRt() {
         ::std::string_view calibration_name = 
            WrappedEnumNameRt<Oddly_Specific_EnumASDOLSAJDPAFHOAF>();
         size_t start = CalculateEnumLeftOffsetRt();
         return calibration_name.size() - start - 58;
      }
      
      /// Normalize a type/enum/function name at runtime                      
      ///   @tparam SRC the token to normalize                                
      ///   @return new literal                                               
      ::std::string NormalizeRt(const Token& SRC) {
         if (not IsASCII(SRC))
            throw ::std::runtime_error {"Token isn't ASCII"};

         ::std::string result {SRC};
         for (const auto& pattern : ReplacePatterns) {
            size_t prev = 0;
            size_t curr = result.find(pattern.what, 0);
            size_t already_replaced = not pattern.with.empty()
               ? result.find(pattern.with, 0)
               : result.npos;
            if (curr == result.npos or not IsTransition(result, curr, curr + pattern.what.size())
            or (already_replaced != result.npos and curr == already_replaced))
               continue;
            
            ::std::string buffer;
            buffer.reserve(result.size());
            do {
               while (curr > prev) {
                  // Copy anything we've skipped                        
                  buffer += result[prev++];
               }

               // Replace                                               
               //buffer.resize(curr + pattern.with.size());
               for (char c : pattern.with)
                  buffer += c;
               prev += pattern.what.size();
               
               curr = result.find(pattern.what, prev);
               already_replaced = not pattern.with.empty()
                  ? result.find(pattern.with, prev)
                  : result.npos;
            }
            while (curr != result.npos
            and   (already_replaced == result.npos or curr != already_replaced));
            
            while (prev < result.size()) {
               // Copy any remaining trailing data                      
               buffer += result[prev++];
            }
            
            result = LglsMov(buffer);
         }
         return result;
      }
      
      /// Get the last, most relevant part of a token that may or may not     
      /// have namespaces in it. Essentially finds last "::" that isn't       
      /// enclosed in a <template>, and skip forward to that.                 
      ///   @param token the token to scan                                    
      ///   @return the last token                                            
      size_t FindLastTokenRt(const Token& token) noexcept {
         size_t depth = 0;
         for (size_t i = token.size() - 1; i < token.size(); --i) {
            switch (token[i]) {
            case ':':
               // If no depth, then we found it                         
               if (not depth)
                  return i + 1;
               break;
            case '>':
               // Open template scope                                   
               ++depth;
               break;
            case '<':
               // Close template scope                                  
               if (depth)
                  --depth;
               break;
            default:
               break;
            }
         }
         return 0;
      }
   }
}