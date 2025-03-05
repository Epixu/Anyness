///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Literal.hpp"
#include "CT/Named.hpp"


namespace Langulus::RTTI
{

   using Lowercase = ::std::string;
   using Token     = ::std::string_view;

   namespace Inner
   {

      /// Types used for pattern matching while isolating typenames           
      /// These need to be in exactly this namespace to avoid corner cases    
      struct Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK {};
      enum { Oddly_Specific_EnumASDOLSAJDPAFHOAF };

      /// Stringifies type T by exploiting the preprocessor                   
      template<class T>
      consteval auto WrappedTypeName() {
         return Literal {LANGULUS_FUNCTION()};
      }

      /// Stringifies value T by exploiting the preprocessor                  
      template<auto T>
      consteval auto WrappedEnumName() {
         return Literal {LANGULUS_FUNCTION()};
      }

      /// Analyze compiler stringification and find the left offset in order  
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the left           
      consteval auto CalculateTypeLeftOffset() {
         constexpr auto calibration_name = 
            WrappedTypeName<Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK>();
         constexpr auto start1 = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");

         if constexpr (start1 != calibration_name.npos)
            return start1;
         else {
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            constexpr auto start2 = calibration_name.find(
               "Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");

            static_assert(start2 != calibration_name.npos);
            return start2;
         }
      }
      constexpr auto CalibratedTypeLeftOffset = CalculateTypeLeftOffset();
      
      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      consteval auto CalculateTypeRightOffset() {
         constexpr auto calibration_name = 
            WrappedTypeName<Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK>();
         constexpr auto start1 = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");

         if constexpr (start1 != calibration_name.npos)
            return calibration_name.size() - start1 - 61;
         else {
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            constexpr auto start2 = calibration_name.find(
               "Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");

            static_assert(start2 != calibration_name.npos);
            return calibration_name.size() - start2 - 38;
         }
      }
      constexpr auto CalibratedTypeRightOffset = CalculateTypeRightOffset();

      /// Analyze compiler stringification and find the left offset in order  
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the left           
      consteval auto CalculateEnumLeftOffset() {
         constexpr auto calibration_name = 
            WrappedEnumName<Oddly_Specific_EnumASDOLSAJDPAFHOAF>();
         constexpr auto start1 = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_EnumASDOLSAJDPAFHOAF");

         if constexpr (start1 != calibration_name.npos)
            return start1;
         else {
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            constexpr auto start2 = calibration_name.find(
               "Oddly_Specific_EnumASDOLSAJDPAFHOAF");

            static_assert(start2 != calibration_name.npos);
            return start2;
         }
      }
      constexpr auto CalibratedEnumLeftOffset = CalculateEnumLeftOffset();

      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      consteval auto CalculateEnumRightOffset() {
         constexpr auto calibration_name = 
            WrappedEnumName<Oddly_Specific_EnumASDOLSAJDPAFHOAF>();
         constexpr auto start1 = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_EnumASDOLSAJDPAFHOAF");

         if constexpr (start1 != calibration_name.npos)
            return calibration_name.size() - start1 - 58;
         else {
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            constexpr auto start2 = calibration_name.find(
               "Oddly_Specific_EnumASDOLSAJDPAFHOAF");

            static_assert(start2 != calibration_name.npos);
            return calibration_name.size() - start2 - 35;
         }
      }
      constexpr auto CalibratedEnumRightOffset = CalculateEnumRightOffset();

      /// Skip all decorations in front and the back of a WrappedTypeName     
      ///   @return the type name                                             
      template<class T>
      consteval auto IsolateTypename() {
         constexpr auto name  = WrappedTypeName<T>();
         constexpr auto size  = name.size();
         constexpr auto left  = CalibratedTypeLeftOffset;
         constexpr auto right = CalibratedTypeRightOffset;
         static_assert(size > left + right, "Invalid type name");
         return name.template substr<left, size - right - left>();
      }

      /// Skip all decorations in front and back of a WrappedEnumName         
      ///   @return the enum name                                             
      template<auto T>
      consteval auto IsolateConstant() {
         constexpr auto name  = WrappedEnumName<T>();
         constexpr auto size  = name.size();
         constexpr auto left  = CalibratedEnumLeftOffset;
         constexpr auto right = CalibratedEnumRightOffset;
         static_assert(size > left + right, "Invalid enum name");
         return name.template substr<left, size - right - left>();
      }

      /// Check if a token transition happens at the beginning and the end of 
      /// a region inside a source. A token transition means, that the token  
      /// is surrounded by non-alphabetical symbols                           
      ///   @param source - data source                                       
      ///   @param lhs - start of the region                                  
      ///   @param rhs - end of the region                                    
      ///   @return true if a transition occurs at both points                
      consteval bool IsTransition(
         const Token& source, std::size_t lhs, std::size_t rhs
      ) {
         return (
               // Test left side for transition                         
               lhs == 0
               or not IsAlpha(source[lhs])
               or     IsAlpha(source[lhs]) != IsAlpha(source[lhs-1])
            ) and (
               // Test right side for transition                        
               rhs == source.size()
               or not IsAlpha(source[rhs-1])
               or     IsAlpha(source[rhs-1]) != IsAlpha(source[rhs])
            );
      }
      
      /// Count the number of found tokens, if separated by non-alphabetical  
      /// symbols                                                             
      consteval std::size_t CountOccurences(const auto& lhs, const auto& rhs) {
         if constexpr (rhs.size() > lhs.size())
            return 0;
         std::size_t occurences = 0;
         std::size_t cookie = 0;
         while ((cookie = lhs.find(rhs, cookie)) != lhs.npos) {
            if (IsTransition(lhs, cookie, cookie + rhs.size()))
               ++occurences;
         }
         return occurences;
      }

      /// Replace all occurences of a substring at compile-time               
      ///   @param source - what are we checking?                             
      ///   @param what - what are we replacing?                              
      ///   @param what - what are we replacing with?                         
      ///   @return new literal                                               
      consteval auto Replace(
         const auto& source, const auto& what, const auto& with
      ) {
         constexpr auto found = CountOccurences(source, what);
         if constexpr (not found)
            return source;
         else {
            Literal<source.size() - found*what.size() + found*with.size()> result;
            std::size_t fill = 0;
            std::size_t prev = 0;
            std::size_t curr = 0;
            while ((curr = source.find(what, prev)) != source.npos) {
               while (curr > prev) {
                  // Copy anything we've skipped                        
                  result[fill++] = source[prev++];
               }

               if (IsTransition(source, curr, curr + what.size())) {
                  // Replace                                            
                  for (auto& c : with)
                     result[fill++] = c;
                  prev += what.size();
               }
            }

            while (prev < source.size()) {
               // Copy any remaining trailing data                      
               result[fill++] = source[prev++];
            }

            return result;
         }
      }
      
      /// Normalize a type/enum/function name                                 
      ///   @param src - the literal to normalize                             
      ///   @return new literal                                               
      consteval auto Normalize(const auto& src) {
         // Replace these patterns when normalizing names               
         // @attention when having similar tokens to replace, order     
         //    them correctly, with longer ones replaced first          
         // @attention replacement will not commence, if IsTransition   
         //    isn't satisifed                                          
         auto a01 = Replace(src, Literal {"*const "}, Literal {"* const"});
         auto a02 = Replace(a01, Literal {" *const"}, Literal {"* const"});
         auto a03 = Replace(a02, Literal {" *"     }, Literal {"*"      });
         auto a04 = Replace(a03, Literal {" &"     }, Literal {"&"      });
         auto a05 = Replace(a04, Literal {" >"     }, Literal {">"      });
         auto a06 = Replace(a05, IsolateTypename<::std::int8_t>(),   Literal {"int8"  });
         auto a07 = Replace(a06, IsolateTypename<::std::int16_t>(),  Literal {"int16" });
         auto a08 = Replace(a07, IsolateTypename<::std::int32_t>(),  Literal {"int32" });
         auto a09 = Replace(a08, IsolateTypename<::std::int64_t>(),  Literal {"int64" });
         auto a10 = Replace(a09, IsolateTypename<::std::uint8_t>(),  Literal {"uint8" });
         auto a11 = Replace(a10, IsolateTypename<::std::uint16_t>(), Literal {"uint16"});
         auto a12 = Replace(a11, IsolateTypename<::std::uint32_t>(), Literal {"uint32"});
         auto a13 = Replace(a12, IsolateTypename<::std::uint64_t>(), Literal {"uint64"});
         auto a14 = Replace(a13, Literal {"class "      }, Literal {""});
         auto a15 = Replace(a14, Literal {"struct "     }, Literal {""});
         auto a16 = Replace(a15, Literal {"enum "       }, Literal {""});
         auto a17 = Replace(a16, Literal {"Langulus::"  }, Literal {""});
         auto a18 = Replace(a17, Literal {"(__cdecl *)" }, Literal {""});
         auto a19 = Replace(a18, Literal {" (*)"        }, Literal {""});
         return a19;
      }

      /// Get the normalized name of a function                               
      ///   @return the normalized token for T at compile-time                
      template<class T>
      consteval auto NameOfFunction() {
         return "Function<" + Normalize(IsolateTypename<T>()) + ">*";
      }
      
      /// Get the normalized name of a type                                   
      ///   @return the normalized token for T at compile-time                
      template<class T>
      consteval auto NameOfType() {
         return Normalize(IsolateTypename<T>());
      }
      
      /// Get the normalized name of a constant                               
      ///   @return the normalized token for T at compile-time                
      template<auto T>
      consteval auto NameOfConstant() {
         constexpr auto fullEnumName = Normalize(IsolateConstant<T>());
         constexpr auto lastNamespace = fullEnumName.find_last_of(':');
         if constexpr (lastNamespace != fullEnumName.npos) {
            constexpr auto lastEnumName = fullEnumName.substr(lastNamespace);
            constexpr auto typeName = NameOfType<decltype(T)>();
            return typeName + "::" + lastEnumName;
         }
         else {
            constexpr auto typeName = NameOfType<decltype(T)>();
            return typeName + "::" + fullEnumName;
         }
      }

      /// Get the last, most relevant part of a token that may or may not     
      /// have namespaces in it. Essentially finds last "::" that isn't       
      /// enclosed in a <template>, and skip forward to that                  
      ///   @param token - the token to scan                                  
      ///   @return the last token                                            
      constexpr ::std::size_t FindLastToken(const Token& token) noexcept {
         ::std::size_t depth = 0;
         for (::std::size_t i = token.size() - 1; i < token.size(); --i) {
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

   } // namespace Langulus::RTTI::Inner
} // namespace Langulus::RTTI

namespace Langulus
{
   
   /// Get the name of a type, templated or not, with consistently named      
   /// template arguments, even if nested, if such are required               
   ///   @tparam T - the type to get the name of                              
   ///   @return the type name                                                
   template<class T>
   consteval auto CppNameOf() {
      if constexpr (::std::is_function_v<Decay<T>>)
         return RTTI::Inner::NameOfFunction<T>();
      else
         return RTTI::Inner::NameOfType<T>();
   }
   
   /// Same as NameOf, but removes all namespaces                             
   ///   @tparam T - the type to get the name of                              
   ///   @return the type name                                                
   template<class T>
   consteval auto LastCppNameOf() {
      if constexpr (::std::is_function_v<Decay<T>>)
         return RTTI::Inner::NameOfFunction<T>();
      else {
         // Find the last ':' symbol, that is not inside <...> scope    
         auto fullName = RTTI::Inner::NameOfType<T>();
         auto lastName = RTTI::Inner::FindLastToken(fullName);
         return fullName.substr(lastName);
      }
   }

   /// Get the name of a named constant                                       
   ///   @tparam E - the constant to get the name of                          
   ///   @return the name of the constant                                     
   template<auto E>
   consteval auto CppNameOf() {
      return RTTI::Inner::NameOfConstant<E>();
   }
   

   /// Same as CppNameOf, but removes all namespaces                          
   ///   @tparam T - the enum to get the name of                              
   ///   @return the name                                                     
   template<auto E>
   consteval auto LastCppNameOf() {
      // Find the last ':' symbol, that is not inside <...> scope       
      auto fullName = RTTI::Inner::NameOfConstant<E>();
      auto lastName = RTTI::Inner::FindLastToken(fullName);
      return fullName.substr(lastName);
   }

   ///                                                                        
   /// NameOf that considers CTTI::Named, or fallbacks to the C++ name        
   /// If you want to avoid custom names, use CppNameOf directly instead      
   ///                                                                        
   template<class T>
   consteval auto NameOf() {
      if constexpr (CT::Named<T>) {
         if constexpr (CTTI::Named<T>::Value)
            // Checked externally, T doesn't have to be complete        
            return CTTI::Named<T>::Name;
         else if constexpr (requires { T::CTTI_Named::Value; })
            // Checked internally, T has to be complete                 
            return T::CTTI_Named::Constant;
         else
            static_assert(false, "Type improperly named");
      }
      else return CppNameOf<T>();
   }
   
   ///                                                                        
   ///   NameOf for enum types and other constants                            
   ///                                                                        
   template<auto E>
   consteval auto NameOf() {
      if constexpr (CT::NamedValue<E>)
         return CTTI::NamedValue<E>::Name;
      else
         return CppNameOf<E>();
   }

} // namespace Langulus
