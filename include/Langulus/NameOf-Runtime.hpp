///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "NameOf.hpp"
#include <source/rtti/ExportRTTI.hpp>


namespace Langulus
{
   using Token = ::std::string_view;
}

namespace Langulus::RTTI
{
   namespace Inner
   {
      /// Check if a name is reserved                                         
      LANGULUS_API(RTTI)
      bool IsReservedRt(const Token& name);

      LANGULUS_API(RTTI)
      ::std::string NormalizeRt(const Token&);

      /// Stringifies type T by exploiting the preprocessor                   
      template<class T>
      ::std::string_view WrappedTypeNameRt() {
         return LANGULUS_FUNCTION();
      }

      /// Stringifies value T by exploiting the preprocessor                  
      template<auto T>
      ::std::string_view WrappedEnumNameRt() {
         return LANGULUS_FUNCTION();
      }

      /// Analyze compiler stringification and find the left offset in order  
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the left           
      LANGULUS_API(RTTI)
      size_t CalculateTypeLeftOffsetRt();
      
      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      LANGULUS_API(RTTI)
      size_t CalculateTypeRightOffsetRt();

      /// Analyze compiler stringification and find the left offset in order  
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the left           
      LANGULUS_API(RTTI)
      size_t CalculateEnumLeftOffsetRt();

      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      LANGULUS_API(RTTI)
      size_t CalculateEnumRightOffsetRt();

      /// Skip all decorations in front and the back of a WrappedTypeName     
      ///   @tparam T the typename to isolate                                 
      ///   @tparam NORMALIZE whether or not to normalize the typename to     
      ///      Langulus specification                                         
      ///   @tparam NAMED whether or not to apply any CTTI named traits       
      ///   @return a compile-time string                                     
      template<class T, bool NAMED = true> LANGULUS(NOINLINE)
      ::std::string IsolateTypenameRt(bool NORMALIZE = true) {
         if constexpr (NAMED and CT::Complete<CTTI::Named<T>>) {
            // Custom name by CTTI::Named specialization                
            if (not IsKeyword(CTTI::Named<T>::Name))
               throw ::std::runtime_error {"Not a valid CTTI::Named - "
               "must be ASCII, starting with an alphabetical symbol, "
               "and must not contain any spaces or operators"};

            if (IsReservedRt(CTTI::Named<T>::Name))
               throw ::std::runtime_error {"Not a valid CTTI::Named - token is reserved"};

            return static_cast<::std::string>(CTTI::Named<T>::Name);
         }
         else if constexpr (::std::is_const_v<T> or ::std::is_volatile_v<T>) {
            // Move `const` next to pointers/references at the end of   
            // type. Discards `volatile` - it shouldn't matter outside  
            // compiler. Helps with better sorting of reflected types   
            auto deptr = IsolateTypenameRt<Decvq<T>, NAMED>(NORMALIZE);
            if constexpr (not ::std::is_const_v<T>)
               return deptr;
            else
               return deptr + " const";
         }
         else if constexpr (::std::is_reference_v<T>) {
            // Append & or const& to the back                           
            auto deptr = IsolateTypenameRt<Decvq<Deref<T>>, NAMED>(NORMALIZE);
            if constexpr (not ::std::is_const_v<Deref<T>>)
               return deptr + "&";
            else
               return deptr + " const&";
         }
         else if constexpr (::std::is_bounded_array_v<T>) {
            // Append extent                                            
            auto deext = IsolateTypenameRt<Deext<T>, NAMED>(NORMALIZE);
            constexpr auto ext = ::std::extent_v<T>;
            static_assert(ext < 1000000, "Extent is too big");
            if constexpr (ext > 99999) {
               return deext + "[" + static_cast<char>('0' + ext/100000)
                                  + static_cast<char>('0' + ext/10000)
                                  + static_cast<char>('0' + ext/1000)
                                  + static_cast<char>('0' + ext/100)
                                  + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";
            }
            else if constexpr (ext > 9999) {
               return deext + "[" + static_cast<char>('0' + ext/10000)
                                  + static_cast<char>('0' + ext/1000)
                                  + static_cast<char>('0' + ext/100)
                                  + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";
            }
            else if constexpr (ext > 999) {
               return deext + "[" + static_cast<char>('0' + ext/1000)
                                  + static_cast<char>('0' + ext/100)
                                  + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";               
            }
            else if constexpr (ext > 99) {
               return deext + "[" + static_cast<char>('0' + ext/100)
                                  + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";               
            }
            else if constexpr (ext > 9) {
               return deext + "[" + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";
            }
            else return deext + "[" + static_cast<char>('0' + ext) + "]";
         }
         else if constexpr (::std::is_pointer_v<T>) {
            // Append * or const* to the back                           
            auto deptr = IsolateTypenameRt<Decvq<Deptr<T>>, NAMED>(NORMALIZE);
            if constexpr (not ::std::is_const_v<Deptr<T>>)
               return deptr + "*";
            else
               return deptr + " const*";
         }
         else if constexpr (NAMED and requires { T::CTTI_Named::Enabled; }) {
            if constexpr (T::CTTI_Named::Enabled) {
               // Custom name taken from T::CTTI_Named member           
               if (not IsKeyword(T::CTTI_Named::Constant))
                  throw ::std::runtime_error {"Not a valid CTTI_Named - "
                  "must be ASCII, starting with an alphabetical symbol, "
                  "and must not contain any spaces or operators"};

               if (IsReservedRt(T::CTTI_Named::Constant))
                  throw ::std::runtime_error {
                  "Not a valid CTTI_Named - token is reserved"};

               return static_cast<::std::string>(T::CTTI_Named::Constant);
            }
            else return IsolateTypenameRt<T, false>(NORMALIZE);
         }
         else {
            // Extract the C++ name, normalize it if required           
            const ::std::string_view name = WrappedTypeNameRt<T>();
            const size_t size = name.size();
            const size_t left = CalculateTypeLeftOffsetRt();
            const size_t right = CalculateTypeRightOffsetRt();
            if (size <= left + right)
               throw ::std::runtime_error {"Invalid type name"};

            const ::std::string_view isolated {name.substr(left, size - right - left)};
            if (IsReservedRt(isolated))
               throw ::std::runtime_error {"Not a valid C++ name - token is reserved"};

            if (not NORMALIZE) {
               if constexpr (::std::is_function_v<T>)
                  return "<" + static_cast<::std::string>(isolated) + ">";
               else
                  return static_cast<::std::string>(isolated);
            }
            else {
               const ::std::string normalized = NormalizeRt(isolated);
               if constexpr (::std::is_function_v<T>)
                  return "<" + normalized + ">";
               else
                  return normalized;
            }
         }
      }

      /// Skip all decorations in front and back of a WrappedEnumName         
      ///   @tparam T the constant to isolate                                 
      ///   @tparam NAMED whether or not to apply any CTTI named traits       
      ///   @param NORMALIZE whether or not to normalize the constant to      
      ///      Langulus specification                                         
      ///   @return a compile-time string                                     
      template<auto T, bool NAMED = true> LANGULUS(NOINLINE)
      ::std::string IsolateConstantRt(bool NORMALIZE = true) {
         if constexpr (NAMED and CT::NamedValue<T>) {
            // Custom name by specializing CTTI::NamedValue             
            if (not IsKeyword(CTTI::NamedValue<T>::Name))
               throw ::std::runtime_error {"Not a valid CTTI::NamedValue - "
               "must be ASCII, starting with an alphabetical symbol, "
               "and must not contain any spaces or operators"};

            if (IsReservedRt(CTTI::NamedValue<T>::Name))
               throw ::std::runtime_error {"Not a valid CTTI::NamedValue - token is reserved"};
               
            return static_cast<::std::string>(CTTI::NamedValue<T>::Name);
         }
         else {
            // Extract the C++ name and normalize it if required        
            const ::std::string_view name = WrappedEnumNameRt<T>();
            const size_t size = name.size();
            const size_t left = CalculateEnumLeftOffsetRt();
            const size_t right = CalculateEnumRightOffsetRt();
            if (size <= left + right)
               throw ::std::runtime_error {"Invalid enum name"};

            ::std::string_view isolated = name.substr(left, size - right - left);
            if (IsReservedRt(isolated))
               throw ::std::runtime_error {"Not a valid C++ value name - token is reserved"};

            if (NORMALIZE)
               return NormalizeRt(isolated);
            else
               return static_cast<::std::string>(isolated);
         }
      }
      
      /// Normalize a type/enum/function name at runtime                      
      ///   @tparam SRC the token to normalize                                
      ///   @return new literal                                               
      LANGULUS_API(RTTI)
      ::std::string NormalizeRt(const Token& SRC);
      
      /// Get the last, most relevant part of a token that may or may not     
      /// have namespaces in it. Essentially finds last "::" that isn't       
      /// enclosed in a <template>, and skip forward to that.                 
      ///   @param token the token to scan                                    
      ///   @return the last token                                            
      LANGULUS_API(RTTI)
      size_t FindLastTokenRt(const Token& token) noexcept;
   }
}

namespace Langulus
{
   /// MARK: CppNameOfRt                                                      
   /// Get the name of a type, templated or not, with consistently named      
   /// template arguments, even if nested, at compile-time                    
   ///   @tparam T the type to get the name of                                
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time and is rarely    
   ///      used for C++ names                                                
   ///   @return a compile-time string                                        
   template<class T, bool NORMALIZE = false>
   ::std::string CppNameOfRt() {
      return RTTI::Inner::IsolateTypenameRt<T, false>(NORMALIZE);
   }
   
   /// Get the name of an enum value at compile-time                          
   ///   @tparam E the constant to get the name of                            
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time and is rarely    
   ///      used for C++ names                                                
   ///   @return a compile-time string                                        
   template<auto E, bool NORMALIZE = false>
   ::std::string CppNameOfRt() {
      return RTTI::Inner::IsolateConstantRt<E, false>(NORMALIZE);
   }

   /// MARK: LastCppNameOfRt                                                  
   /// Same as CppNameOf, but removes all namespaces at compile-time          
   ///   @tparam T the type to get the name of                                
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time and is rarely    
   ///      used for C++ names                                                
   ///   @return a compile-time string                                        
   template<class T, bool NORMALIZE = false>
   ::std::string LastCppNameOfRt() {
      // Find the last ':' symbol, that is not inside <...> scope       
      auto fullName = RTTI::Inner::IsolateTypenameRt<T, false>(NORMALIZE);
      auto lastName = RTTI::Inner::FindLastTokenRt(fullName);
      return fullName.substr(lastName);
   }

   /// Same as CppNameOf, but removes all namespaces at compile-time          
   ///   @tparam E the enum to get the name of                                
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time and is rarely    
   ///      used for C++ names                                                
   ///   @return a compile-time string                                        
   template<auto E, bool NORMALIZE = false>
   ::std::string LastCppNameOfRt() {
      // Find the last ':' symbol, that is not inside <...> scope       
      auto fullName = RTTI::Inner::IsolateConstantRt<E, false>(NORMALIZE);
      auto lastName = RTTI::Inner::FindLastTokenRt(fullName);
      return fullName.substr(lastName);
   }

   /// MARK: NameOfRt                                                         
   /// Get the name of a type at compile-time.                                
   /// Considers CTTI::Named, or fallbacks to the C++ name.                   
   /// If you want to avoid custom names, use CppNameOf directly instead.     
   ///   @attention similarly named types in anonymous namespaces will result 
   ///      in the same name. If this is not desired disable NORMALIZE, or    
   ///      specialize CTTI::Named for each translation unit the type         
   ///      appears in                                                        
   ///   @tparam T the type to get the name of                                
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time, so you might    
   ///      want to do it at runtime instead if build time becomes an issue   
   ///      See Langulus::RTTI::Inner::NormalizeAtRuntime                     
   ///   @return a compile-time string                                        
   ///   @return a compile-time string                                        
   template<class T, bool NORMALIZE = true>
   ::std::string NameOfRt() {
      return RTTI::Inner::IsolateTypenameRt<T>(NORMALIZE);
   }
   
   /// Get the name of an enum value at compile-time.                         
   /// Considers CTTI::NamedValue, or fallbacks to the C++ name.              
   ///   @attention similarly named values in anonymous namespaces will result
   ///      in the same name. If this is not desired disable NORMALIZE, or    
   ///      specialize CTTI::NamedValue for each translation unit the value   
   ///      appears in                                                        
   ///   @tparam E the value to get the name of                               
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time, so you might    
   ///      want to do it at runtime instead if build time becomes an issue   
   ///      See Langulus::RTTI::Inner::NormalizeAtRuntime                     
   ///   @return a compile-time string                                        
   template<auto E, bool NORMALIZE = true>
   ::std::string NameOfRt() {
      return RTTI::Inner::IsolateConstantRt<E>(NORMALIZE);
   }
}
