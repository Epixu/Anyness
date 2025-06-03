#pragma once
#include <array>
#include <functional>
#include <iterator>
//#include <ostream>
#include <string_view>
#include <type_traits>


namespace Langulus
{
   namespace CT
   {
      
      /// Check if all T are Literal types                                    
      template<class...T>
      concept FixedString = (T::CTTI_StringLiteral and ...);

      /// Supported character types used by Literal                           
      template<class...T>
      concept FixedChar = ((
              std::same_as<T, char>
           or std::same_as<T, signed char>
           or std::same_as<T, unsigned char>
           or std::same_as<T, wchar_t>
           or std::same_as<T, char8_t>
           or std::same_as<T, char16_t>
           or std::same_as<T, char32_t>
         ) and ...);

   } // namespace Langulus::CT

   using Token = ::std::string_view;


   ///                                                                        
   /// String literal                                                         
   /// You can use it as a template parameter                                 
   /// Should be introduced in C++26 as std::fixed_string, supposedly         
   ///                                                                        
   template<class T, ::std::size_t N, class TRAITS = ::std::char_traits<T>>
   struct Literal {
      static constexpr bool CTTI_StringLiteral = true;

      using storage_type = std::array<T, N + 1>;
      storage_type _data{};

      using traits_type = TRAITS;
      using value_type = T;
      using pointer = value_type*;
      using const_pointer = const value_type*;
      using reference = value_type&;
      using const_reference = const value_type&;
      using iterator = typename storage_type::iterator;
      using const_iterator = typename storage_type::const_iterator;
      using reverse_iterator = typename storage_type::reverse_iterator;
      using const_reverse_iterator = typename storage_type::const_reverse_iterator;
      using size_type = size_t;
      using difference_type = ptrdiff_t;
      using view_type = ::std::basic_string_view<value_type, traits_type>;

      static constexpr ::std::size_t npos = view_type::npos;
      static constexpr ::std::size_t Count = N;
      static constexpr bool Empty = (N == 0);

      constexpr Literal() noexcept = default;

      constexpr Literal(const value_type(&array)[N + 1]) noexcept {
         std::copy(std::begin(array), std::end(array), _data.begin());
      }

      constexpr Literal& operator = (const value_type(&array)[N + 1]) noexcept {
         std::copy(std::begin(array), std::end(array), _data.begin());
         return *this;
      }

      ///                                                                     
      /// Iteration                                                           
      ///                                                                     
      template<class Self>
      constexpr auto begin(this Self&& self) noexcept {
         return self._data.begin();
      }

      template<class Self>
      constexpr auto end(this Self&& self) noexcept {
         return self._data.end() - 1;
      }

      constexpr auto cbegin() const noexcept {
         return _data.cbegin();
      }

      constexpr auto cend() const noexcept {
         return _data.cend() - 1;
      }

      template<class Self>
      constexpr auto rbegin(this Self&& self) noexcept {
         return self._data.rbegin() + 1;
      }

      template<class Self>
      constexpr auto rend(this Self&& self) noexcept {
         return self._data.rend();
      }

      constexpr auto crbegin() const noexcept {
         return _data.crbegin() + 1;
      }

      constexpr auto crend() const noexcept {
         return _data.crend();
      }

      ///                                                                     
      /// Encapsulation                                                       
      ///                                                                     
      consteval auto size()     const noexcept { return Count; }
      consteval auto length()   const noexcept { return Count; }
      consteval auto max_size() const noexcept { return Count; }
      consteval auto empty()    const noexcept { return Empty; }

      ///                                                                     
      /// Access                                                              
      ///                                                                     
      template<class Self>
      constexpr decltype(auto) operator [] (this Self&& self, size_type n) {
         return self._data[n];
      }

      template<class Self>
      constexpr decltype(auto) at(this Self&& self, size_type n) {
         return self._data.at(n);
      }

      template<class Self>
      constexpr decltype(auto) front(this Self&& self) noexcept requires (not Empty) {
         return self._data.front();
      }

      template<class Self>
      constexpr decltype(auto) back(this Self&& self) noexcept requires (not Empty) {
         return self._data[Count - 1];
      }

      template<class Self>
      constexpr auto data(this Self&& self) noexcept {
         return self._data.data();
      }

      constexpr auto c_str() const noexcept {
         return _data.data();
      }

      ///                                                                     
      /// Retype                                                              
      ///                                                                     
      /// Get a resized Literal with the same properties                      
      template<size_t M>
      using Resized = Literal<value_type, M, traits_type>;

   protected:
      template<class, ::std::size_t, class>
      friend struct Literal;

      template<size_type pos, size_type count, size_type size>
      consteval static size_type calculate_substr_size() {
         if constexpr (pos >= size)
            return 0;

         constexpr size_type rcount = std::min(count, size - pos);
         return rcount;
      }

      template <size_type pos, size_type count>
      using substr_result_type = Resized<calculate_substr_size<pos, count, N>()>;

      constexpr view_type sv() const { return *this; }

   public:

      /// Implicit cast to a string view                                      
      constexpr operator view_type() const noexcept {
         return {data(), N };
      }

      /// Get a region of the string                                          
      template<size_type pos = 0, size_type count = npos> requires (pos <= N)
      constexpr auto substr() const noexcept {
         substr_result_type<pos, count> result;
         std::copy(begin() + pos, begin() + pos + result.size(), result.begin());
         return result;
      }

      /// Find                                                                
      template <size_t M>
      constexpr size_type find(const Resized<M>& str, size_type pos = 0) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find(str.sv(), pos);
      }
      constexpr size_type find(view_type sv, size_type pos = 0) const noexcept {
         return sv().find(sv, pos);
      }
      constexpr size_type find(const value_type* s, size_type pos, size_type n) const {
         return sv().find(s, pos, n);
      }
      constexpr size_type find(const value_type* s, size_type pos = 0) const {
         return sv().find(s, pos);
      }
      constexpr size_type find(value_type c, size_type pos = 0) const noexcept {
         return sv().find(c, pos);
      }

      /// Find in reverse                                                     
      template <size_t M>
      constexpr size_type rfind(const Resized<M>& str, size_type pos = npos) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().rfind(str.sv(), pos);
      }
      constexpr size_type rfind(view_type sv, size_type pos = npos) const noexcept {
         return sv().rfind(sv, pos);
      }
      constexpr size_type rfind(const value_type* s, size_type pos, size_type n) const {
         return sv().rfind(s, pos, n);
      }
      constexpr size_type rfind(const value_type* s, size_type pos = npos) const {
         return sv().rfind(s, pos);
      }
      constexpr size_type rfind(value_type c, size_type pos = npos) const noexcept {
         return sv().rfind(c, pos);
      }

      /// Find the first of                                                   
      template <size_t M>
      constexpr size_type find_first_of(const Resized<M>& str, size_type pos = 0) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find_first_of(str.sv(), pos);
      }
      constexpr size_type find_first_of(view_type sv, size_type pos = 0) const noexcept {
         return sv().find_first_of(sv, pos);
      }
      constexpr size_type find_first_of(const value_type* s, size_type pos, size_type n) const {
         return sv().find_first_of(s, pos, n);
      }
      constexpr size_type find_first_of(const value_type* s, size_type pos = 0) const {
         return sv().find_first_of(s, pos);
      }
      constexpr size_type find_first_of(value_type c, size_type pos = 0) const noexcept {
         return sv().find_first_of(c, pos);
      }

      /// Find the last of                                                    
      template <size_t M>
      constexpr size_type find_last_of(const Resized<M>& str, size_type pos = npos) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find_last_of(str.sv(), pos);
      }
      constexpr size_type find_last_of(view_type sv, size_type pos = npos) const noexcept {
         return sv().find_last_of(sv, pos);
      }
      constexpr size_type find_last_of(const value_type* s, size_type pos, size_type n) const {
         return sv().find_last_of(s, pos, n);
      }
      constexpr size_type find_last_of(const value_type* s, size_type pos = npos) const {
         return sv().find_last_of(s, pos);
      }
      constexpr size_type find_last_of(value_type c, size_type pos = npos) const noexcept {
         return sv().find_last_of(c, pos);
      }

      /// Find the first NOT of                                               
      template <size_t M>
      constexpr size_type find_first_not_of(const Resized<M>& str, size_type pos = 0) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find_first_not_of(str.sv(), pos);
      }
      constexpr size_type find_first_not_of(view_type sv, size_type pos = 0) const noexcept {
         return sv().find_first_not_of(sv, pos);
      }
      constexpr size_type find_first_not_of(const value_type* s, size_type pos, size_type n) const {
         return sv().find_first_not_of(s, pos, n);
      }
      constexpr size_type find_first_not_of(const value_type* s, size_type pos = 0) const {
         return sv().find_first_not_of(s, pos);
      }
      constexpr size_type find_first_not_of(value_type c, size_type pos = 0) const noexcept {
         return sv().find_first_not_of(c, pos);
      }

      /// Find the last NOT of                                                
      template <size_t M>
      constexpr size_type find_last_not_of(const Resized<M>& str, size_type pos = npos) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find_last_not_of(str.sv(), pos);
      }
      constexpr size_type find_last_not_of(view_type sv, size_type pos = npos) const noexcept {
         return sv().find_last_not_of(sv, pos);
      }
      constexpr size_type find_last_not_of(const value_type* s, size_type pos, size_type n) const {
         return sv().find_last_not_of(s, pos, n);
      }
      constexpr size_type find_last_not_of(const value_type* s, size_type pos = npos) const {
         return sv().find_last_not_of(s, pos);
      }
      constexpr size_type find_last_not_of(value_type c, size_type pos = npos) const noexcept {
         return sv().find_last_not_of(c, pos);
      }

      /// Compare                                                             
      constexpr int compare(view_type v) const noexcept {
         return sv().compare(v);
      }
      constexpr int compare(size_type pos1, size_type count1, view_type v) const {
         return sv().compare(pos1, count1, v);
      }
      constexpr int compare(size_type pos1, size_type count1, view_type v, size_type pos2, size_type count2) const {
         return sv().compare(pos1, count1, v, pos2, count2);
      }
      constexpr int compare(const value_type* s) const {
         return sv().compare(s);
      }
      constexpr int compare(size_type pos1, size_type count1, const value_type* s) const {
         return sv().compare(pos1, count1, s);
      }
      constexpr int compare(size_type pos1, size_type count1, const value_type* s, size_type count2) const {
         return sv().compare(pos1, count1, s, count2);
      }

      /// Starts with                                                         
      constexpr bool starts_with(view_type v) const noexcept {
         return sv().substr(0, v.size()) == v;
      }
      constexpr bool starts_with(char c) const noexcept {
         return !empty() && traits_type::eq(front(), c);
      }
      constexpr bool starts_with(const value_type* s) const noexcept {
         return starts_with(view_type(s));
      }

      /// Ends with                                                           
      constexpr bool ends_with(view_type sv) const noexcept {
         return size() >= sv.size() && compare(size() - sv.size(), npos, sv) == 0;
      }
      constexpr bool ends_with(value_type c) const noexcept {
         return !empty() && traits_type::eq(back(), c);
      }
      constexpr bool ends_with(const value_type* s) const {
         return ends_with(view_type(s));
      }

      /// Contains                                                            
      constexpr bool contains(view_type sv) const noexcept {
         return find(sv) != npos;
      }
      constexpr bool contains(value_type c) const noexcept {
         return find(c) != npos;
      }
      constexpr bool contains(const value_type* s) const {
         return find(s) != npos;
      }

      void swap(Literal& other) noexcept(std::is_nothrow_swappable_v<storage_type>) {
         _data.swap(other._data);
      }
   };

   template<class TChar, size_t N>
   Literal(const TChar(&)[N]) -> Literal<TChar, N - 1>;


   /// Swap two literals                                                      
   template<CT::FixedString S>
   void swap(S& lhs, S& rhs) noexcept(noexcept(lhs.swap(rhs))) {
      lhs.swap(rhs);
   }


   ///                                                                        
   /// Literal == Literal                                                     
   constexpr bool operator == (
      const CT::FixedString auto& lhs,
      const CT::FixedString auto& rhs
   ) {
      if constexpr (lhs.size() != rhs.size())
         return false;
      else for (size_t i = 0; i < lhs.size(); ++i) {
         if (lhs[i] != rhs[i])
            return false;
      }
      return true;
   }

   /// Literal == View                                                        
   template<CT::FixedString S>
   constexpr bool operator == (const S& lhs, typename S::view_type rhs) {
      return static_cast<typename S::view_type>(lhs) == rhs;
   }

   /// View == Literal                                                        
   template<CT::FixedString S>
   constexpr bool operator == (typename S::view_type lhs, const S& rhs) {
      return lhs == static_cast<typename S::view_type>(rhs);
   }

   /// Literal == Array                                                       
   template<CT::FixedString S, ::std::size_t N>
   constexpr bool operator == (const S& lhs, const typename S::value_type(&rhs)[N]) {
      if constexpr (S::Count != N - 1)
         return false;
      else for (size_t i = 0; i < N; ++i) {
         if (lhs[i] != rhs[i])
            return false;
      }
      return true;
   }

   /// Array == Literal                                                       
   template<CT::FixedString S, ::std::size_t N>
   constexpr bool operator == (const typename S::value_type(&lhs)[N], const S& rhs) {
      return rhs == lhs;
   }


   ///                                                                        
   /// Literal <=> Literal                                                    
   constexpr auto operator <=> (
      const CT::FixedString auto& lhs,
      const CT::FixedString auto& rhs
   ) {
      using lhs_type = std::decay_t<decltype(lhs)>;
      using sv_type = typename lhs_type::view_type;
      return static_cast<sv_type>(lhs) <=> rhs;
   }

   /// Literal <=> View                                                       
   template<CT::FixedString S>
   constexpr auto operator <=> (const S& lhs, const typename S::view_type& rhs) {
      return static_cast<typename S::view_type>(lhs) <=> rhs;
   }
   
   /// View <=> Literal                                                       
   template<CT::FixedString S>
   constexpr auto operator <=> (const typename S::view_type& lhs, const S& rhs) {
      return lhs <=> static_cast<typename S::view_type>(rhs);
   }
   
   /// Literal <=> Array                                                      
   template<CT::FixedString S, ::std::size_t N>
   constexpr auto operator <=> (const S& lhs, const typename S::value_type(&rhs)[N]) {
      using sv_type = typename S::view_type;
      return static_cast<sv_type>(lhs) <=> sv_type {rhs};
   }
   
   /// Array <=> Literal                                                      
   template<CT::FixedString S, ::std::size_t N>
   constexpr auto operator <=> (const typename S::value_type(&lhs)[N], const S& rhs) {
      using sv_type = typename S::view_type;
      return sv_type {lhs} <=> static_cast<sv_type>(rhs);
   }
   

   ///                                                                        
   /// Concatenation                                                          
   ///                                                                        
   template<CT::FixedString LHS, CT::FixedString RHS>
   constexpr auto operator + (const LHS& lhs, const RHS& rhs) {
      typename LHS::template Resized<LHS::Count + RHS::Count> result;
      size_t i = 0;
      for (; i < LHS::Count; ++i)
         result[i] = lhs[i];
      for (; i < LHS::Count + RHS::Count; ++i)
         result[i] = rhs[i - LHS::Count];
      return result;
   }

   template<CT::FixedChar C, size_t N>
   constexpr auto operator + (const C(&lhs)[N], const CT::FixedString auto& rhs) {
      Literal lhs2 = lhs;
      return lhs2 + rhs;
   }

   template<CT::FixedChar C, size_t N>
   constexpr auto operator + (const CT::FixedString auto& lhs, const C(&rhs)[N]) {
      Literal rhs2 = rhs;
      return lhs + rhs2;
   }

   namespace Inner
   {

      template<class T>
      constexpr auto from_char(T ch) {
         Literal<T, 1> fs;
         fs[0] = ch;
         return fs;
      }

   } // namespace Langulus::Inner

   constexpr auto operator + (CT::FixedChar auto lhs, const CT::FixedString auto& rhs) {
      return Inner::from_char(lhs) + rhs;
   }

   constexpr auto operator + (const CT::FixedString auto& lhs, CT::FixedChar auto rhs) {
      return lhs + Inner::from_char(rhs);
   }

   /*template<class TChar, size_t N, class TTraits>
   auto& operator << (
      std::basic_ostream<TChar, TTraits>& out,
      const Literal<TChar, N, TTraits>& str
   ) {
      out << str.data();
      return out;
   }*/

   /// Equivalent to Yes, but also carries a string literal                   
   template<Literal TEXT>
   struct YesText {
      static constexpr Literal Constant = TEXT;
      static constexpr bool Enabled = true;
   };

} // namespace Langulus



namespace std
{

   ///                                                                        
   /// Hash support                                                           
   ///                                                                        
   template<class TChar, size_t N>
   struct hash<Langulus::Literal<TChar, N>> {
      using argument_type = Langulus::Literal<TChar, N>;

      size_t operator()(const argument_type& str) const {
         using sv_t = typename argument_type::string_view_type;
         return std::hash<sv_t>()(static_cast<sv_t>(str));
      }
   };

} // namespace std